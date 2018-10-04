{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module WordFreq (
  loadWikiDump, WordFreq, filterWordFreq, foldWordFreq, addWord
, loadWordList, knownWord
) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (adjust, filterWithKey, foldMapWithKey, insert, member, unionWith)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Data.Text (Text)
import qualified Data.Text as Text (copy, lines, pack, toLower)
import qualified Data.Text.IO as Text (readFile, hGetChunk)
import System.IO (withFile, IOMode(ReadMode))
import GHC.Base (($!))

wikiDump :: Parser (WordFreq Text)
wikiDump = loop mempty where
  loop ws = ifM atEnd (pure ws) $ do
    w <- dirt *> takeWhile1 isWordy <* dirt
    loop $! addWord w ws
  dirt = many' $ startDoc <|> endDoc <|> notWordy
  startDoc = string "<doc " >> skipWhile (not . isEndOfLine) >> endOfLine
  endDoc   = string "</doc>" >> endOfLine
  notWordy = void $ satisfy $ not . isWordy
  isWordy = (||) <$> isAlpha <*> (== '-')

loadWikiDump :: FilePath -> IO (WordFreq Text)
loadWikiDump fp = withFile fp ReadMode $ \h ->
  fromMaybe mempty . maybeResult <$> parseWith (Text.hGetChunk h) wikiDump ""

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

loadWordList :: FilePath -> IO (Set Text)
loadWordList = fmap (Set.fromList . map Text.toLower . Text.lines)
             . Text.readFile

knownWord ws w = Set.member (Text.toLower w) ws
