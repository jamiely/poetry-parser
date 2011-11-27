module PoemClassifier (
  classify, 
  PoemClassifier.test) where

import CMUPronouncingDictionary
import PoemAnalyzer
import PoemParser
import Test.HUnit
import Data.List.Split(splitOn)

-- | Use to classify a poem. Given a poem, and dictionary, this will output information
-- about its type, rhyming scheme, and meter.
classify :: String -> String -> String
classify poem dictText = poemDescription where
  dict = loadWords dictText $ wordList poem
  words = justWords $ getWords (lines poem) dict
  poemDescription = analyze words

testClassify :: Test 
testClassify = "Test classify" ~: TestList [
  -- fails right now
  classify "something something fox\nsomething something pox" "SOMETHING  S AH1 M TH IH0 NG\nFOX  F AA1 K S\nPOX  P AA1 K S" ~?= "Rhyming poem aa"
  ]

-- | Returns a description of the poem
analyze :: [[Word]] -> String
analyze _ = error "unimplemented"

testAnalyze :: Test
testAnalyze = "Test analyze" ~: TestList [
  analyze [[testWordFox],[testWordPox]] ~?= "Rhyming poem: aa"
  ]



test :: IO ()
test = do
  runTestTT (TestList [
    testAnalyze,
    testClassify
    ])
  return ()
