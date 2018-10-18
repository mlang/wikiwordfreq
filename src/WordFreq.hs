{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module WordFreq (
  loadWikiDump, WordFreq, filterWordFreq, foldWordFreq, addWord
, commonSequences, normalize, byFrequency
, loadWordList, knownWord
) where
import Debug.Trace (traceShow)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Control.Parallel.Strategies
import Data.Attoparsec.Text hiding (take)
import Data.Char (isAlpha)
import Data.Foldable (foldl')
import Data.List (groupBy, sort, sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Ord (Down(Down), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Base (($!))
import System.IO (withFile, IOMode(ReadMode))

wikiDump :: WordFreq Text -> Parser (WordFreq Text)
wikiDump ws = ifM atEnd (pure ws) $ do
  w <- dirt *> takeWhile1 isWordy <* dirt
  wikiDump $! addWord w ws
 where
  dirt = many' $ startDoc <|> endDoc <|> notWordy
  startDoc = string "<doc " >> skipWhile (not . isEndOfLine) >> endOfLine
  endDoc   = string "</doc>" >> endOfLine
  notWordy = void $ satisfy $ not . isWordy
  isWordy = (||) <$> isAlpha <*> (== '-')

loadWikiDump :: WordFreq Text -> FilePath -> IO (WordFreq Text)
loadWikiDump wc fp = withFile fp ReadMode $ \h ->
  fromMaybe mempty . maybeResult <$> parseWith (Text.hGetChunk h) (wikiDump wc) ""

newtype WordFreq a = WordFreq { unWordFreq :: Map a Int } deriving (Eq)
instance Ord a => Semigroup (WordFreq a) where
  WordFreq x <> WordFreq y = WordFreq $ Map.unionWith (+) x y
instance Ord a => Monoid (WordFreq a) where
  mempty = WordFreq mempty

filterWordFreq f g = WordFreq . Map.filterWithKey (\k v -> f k && g v) . unWordFreq
foldWordFreq f = Map.foldMapWithKey f . unWordFreq
addWord w = WordFreq . add . unWordFreq where
  add m | Map.member w m = Map.adjust (+ 1) w m
        | otherwise = Map.insert (Text.copy w) 1 m

normalize :: WordFreq Text -> WordFreq Text
normalize = WordFreq
          . Map.fromList . Map.elems . fmap merge . Map.foldrWithKey' lc mempty
          . unWordFreq where
  lc k a = Map.insertWith mappend (Text.toLower k) [(k, a)]
  merge xs = let ((k, a):xs') = sortOn (Down . snd) xs 
                 !a' = a + sum (snd <$> xs')
             in (k, a')

commonSequences :: WordFreq Text -> WordFreq Text
commonSequences = WordFreq
                . deleteInfix . Map.foldrWithKey' subseqs mempty
                . unWordFreq where
  subseqs :: Text -> Int -> Map Text Int -> Map Text Int
  subseqs k a !m = let lc = Text.toLower k
                       l = Text.length lc
                   in foldl' (flip $ uncurry $ Map.insertWith (+)) m [
                        (Text.take n $ Text.drop i lc, a)
                      | i <- [0  .. l - 1], n <- [1 .. l - i]
                      ]
  deleteInfix m = Map.withoutKeys m $ mconcat $
                  withStrategy (parList rdeepseq) $ map search $
                  foldMap (breakBy $ Down . Text.length) $
                  Map.foldrWithKey' invert mempty m where
    invert k a = IntMap.insertWith mappend a [k]
    search (l, r) = Set.fromList $ concatMap (\x -> filter (`Text.isInfixOf` x) r) l
    breakBy by = go . sortOn fst . map (\a -> (by a, a)) where
      chunksOf i ls = map (take i) (splitter ls) where
        splitter [] = []
        splitter l  = l : splitter (drop i l)
      go [] = []
      go ((b, a):xs) = case break ((b /=) . fst) xs of
        (_, []) -> []
        (xs', ys) -> let !ys' = map snd ys
                         pieces = chunksOf 1000 $ a:map snd xs'
                     in map (, ys') pieces ++ go ys

byFrequency :: WordFreq Text -> [(Int, Text)]
byFrequency = IntMap.foldlWithKey' g []
            . withStrategy (parTraversable rdeepseq) . fmap sort
            . Map.foldrWithKey' f mempty . unWordFreq where
  f k a = IntMap.insertWith (const (k:)) a [k]
  g xs a ks = map (a,) ks ++ xs

loadWordList :: FilePath -> IO (Set Text)
loadWordList = fmap (Set.fromList . map Text.toLower . Text.lines)
             . Text.readFile

knownWord ws w = Set.member (Text.toLower w) ws
