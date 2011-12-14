module PoemClassifier (
  classify, 
  PoemClassifier.test) where

import CMUPronouncingDictionary
import PoemAnalyzer
import PoemParser
import Test.HUnit

-- | Use to classify a poem. Given a poem, and dictionary, 
-- this will output information about its type, rhyming scheme, and meter.
classify :: String -> String -> String
classify poem dictText = poemDescription where
  dict = loadWords dictText $ words poem
  res = justWords $ getWords (lines poem) dict
  poemDescription = analyze res

testClassify :: Test 
testClassify = "Test classify" ~: TestList [
  -- fails right now
  classify "something something fox\nsomething something pox" "SOMETHING  S AH1 M TH IH0 NG\nFOX  F AA1 K S\nPOX  P AA1 K S" ~?= "Rhyming poem aa"
  ]

-- | Returns a description of the poem
analyze :: [PoemLine] -> String
-- Tokenize the words, and parse using one of the available parsers
analyze poem = let toks = PoemParser.lex poem in
  case doParse haiku toks of
    [_] -> "haiku"
    []               -> "unsupported form, or junk" 

testAnalyze :: Test
testAnalyze = "Test analyze" ~: TestList [
  analyze [[testWordFox],[testWordPox]] ~?= "Rhyming poem: aa"
  ]

supportedParsers :: [PoemParser RhymeMap]
supportedParsers = [haiku]

test :: IO ()
test = do
  runTestTT (TestList [
    testAnalyze,
    testClassify
    ])
  return ()
