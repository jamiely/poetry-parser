{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Main (main) where

import System.IO
import PoemClassifier(classify)
import Data.List
import Data.Char(toUpper, isAlpha)

main :: IO ()
main = do
  -- Read the poem in from STDIN
  inh <- openFile "dictionary/cmudict.0.7a" ReadMode
  putStrLn "Opened file"
  poem <- getContents
  loadDictionary poem inh (map (filter isAlpha) (wordList poem)) "" 

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

runClassifier :: String -> String -> IO ()
runClassifier poem dict = do
  -- echo the poem and its type
  putStrLn $ "Poem: \n" ++ poem
  putStrLn $ "Dictionary: \n" ++ dict
  putStrLn $ "Type: " ++ classify poem dict

