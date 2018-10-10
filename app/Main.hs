{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM)
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath.Glob (glob)
import System.Environment (getArgs)
import WordFreq
import Options.Applicative

data Options = Options {
  knownWordListFiles :: [FilePath]
, knownOutput :: Maybe FilePath
, unknownOutput :: Maybe FilePath
, subsequenceOutput :: Maybe FilePath
, wikiExtractorFiles :: [FilePath]
} deriving (Eq)

programOptions :: Parser Options
programOptions =
  Options <$> many (strOption $ long "known-words" <> short 'w' <> metavar "FILE")
          <*> optional (strOption $ long "known" <> short 'k' <> metavar "KNOWN-OUTPUT-FILE")
          <*> optional (strOption $ long "unknown" <> short 'u' <> metavar "UNKNOWN-OUTPUT-FILE")
          <*> optional (strOption $ long "subseq" <> short 's' <> metavar "OUTPUT-FILE")
          <*> some (argument str (metavar "FILES..."))

wikiwc :: Options -> IO ()
wikiwc Options{..} = do
  words <- foldl' mappend mempty <$> mapConcurrently loadWordList knownWordListFiles
  wc <- normalize <$> foldM loadWikiDump mempty wikiExtractorFiles
  let known = filterWordFreq (knownWord words) (const True) wc
  let unknown = filterWordFreq (not . knownWord words) (const True) wc
  case knownOutput of
    Just fp -> Text.writeFile fp $ toText . byFrequency $ known
    Nothing -> pure ()
  case unknownOutput of
    Just fp -> Text.writeFile fp $ toText . byFrequency $ unknown
    Nothing -> pure ()
  case subsequenceOutput of
    Just fp -> Text.writeFile fp $ toText . byFrequency $ commonSequences known
    Nothing -> pure ()
  pure ()

main :: IO ()
main = wikiwc =<< execParser opts where
  opts = info (programOptions <**> helper) $
       fullDesc
    <> progDesc "Count words and substrings from Wikipedia dumps."
    <> header "wikiwc - Wikipedia word counter"

toText :: [(Int, Text)] -> Text
toText = foldMap f where
  f (a, w) = Text.pack (show a) <> " " <> w <> "\n"
