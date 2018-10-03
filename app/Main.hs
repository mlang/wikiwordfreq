{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async (mapConcurrently)
import Data.Text (Text)
import qualified Data.Text as Text (lines, null, pack, toLower, words)
import qualified Data.Text.IO as Text (putStr)
import System.FilePath.Glob (glob)
import System.Environment (getArgs)
import WordFreq

main :: IO ()
main = do
  args <- getArgs
  case args of
    [glob] -> run glob
    _ -> run "wiki_*"

run match = do
  words <- loadWordList "/usr/share/dict/ngerman" 
  map <- mconcat <$> (mapConcurrently loadWikiDump =<< glob match)
  let known = filterWordFreq (knownWord words) (const True) map
  let unknown = filterWordFreq (not . knownWord words) (> 1) map
  Text.putStr $ foldWordFreq showWord known

showWord :: Text -> Int -> Text
showWord s i = Text.pack (show i) <> " " <> s <> "\n"
