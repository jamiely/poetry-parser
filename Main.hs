{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans -fno-warn-unused-do-bind #-}

--Advanced Programming, final project
--by Jamie Ly <jamiely>, Nicholas McAvoy <mcavoyna>

module Main (main) where

import System.IO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import PoemClassifier(classify)
import Data.List
import Data.Char(toUpper, isAlpha)
import Control.Monad (filterM)

main :: IO ()
main = do
  -- Read the poem in from STDIN
  dictPath <- chooseDictionaryPath
  putStrLn $ "Using dictionary: " ++ dictPath
  inh <- openFile dictPath ReadMode
  poem <- getContents
  loadDictionary poem inh (map (filter isAlpha) (wordList poem)) ""

-- | Finds a usable dictionary file.
-- Priority: $CMUDICT_PATH, extra/cmudict.0.7a, dictionary-small.txt
chooseDictionaryPath :: IO FilePath
chooseDictionaryPath = do
  envPath <- lookupEnv "CMUDICT_PATH"
  let candidates = maybe id (:) envPath ["extra/cmudict.0.7a", "dictionary-small.txt"]
  existing <- filterM doesFileExist candidates
  case existing of
    (p:_) -> return p
    [] -> ioError (userError "No dictionary found. Set CMUDICT_PATH or add extra/cmudict.0.7a")

-- | Splits the string into unique, upper-cased words
wordList :: String -> [String]
wordList str = map (map toUpper) $ sort $ nub $ concatMap words (lines str) 

-- | Given a dictionary filename, and list of words, excerpts of the dictionary
-- will be returned corresponding to the list of words
loadDictionary :: String -> Handle -> [String] -> String -> IO ()
loadDictionary poem inh wrds excerpt = do
  ineof <- hIsEOF inh
  if ineof || null wrds 
  then runClassifier poem excerpt
  else do
      line <- hGetLine inh
      if null (prefixes line) 
      then loadDictionary poem inh wrds excerpt 
      else loadDictionary poem inh (delete (prefix line) wrds) 
                                   (excerpt ++ ('\n':line)) where
        prefixes l = filter (\word -> isPrefixOf word l) wrds
        prefix l = head $ prefixes l

-- | Classifies a poem using the passed dictionary string
runClassifier :: String -> String -> IO ()
runClassifier poem dict = do
  -- echo the poem and its type
  putStrLn $ "Poem: \n" ++ poem
  putStrLn $ "Dictionary: \n" ++ dict
  putStrLn $ "Type: " ++ classify poem dict

