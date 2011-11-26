module PoemClassifier (
  classify, 
  PoemClassifier.test) where

import CMUPronouncingDictionary
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

-- | Given a String, splits string on spaces, removing empty strings
wordList :: String -> [String]
wordList text = filter (/= "") $ splitOn " " text

-- | Use to determine if two words rhyme. Compares the last 3 phonemes
-- of each word to determine if they rhyme.
rhymes :: Word -> Word -> Bool
rhymes (Word _ _ _ ph1) (Word _ _ _ ph2) = part ph1 == part ph2 where
  part = (take 3) . reverse

testRhymes :: Test
testRhymes = "Test Rhymes" ~: TestList [
  "-zion" ~: rhymes testWordTransfusion testWordVision ~?= True,
  "-ox" ~: rhymes testWordFox testWordPox ~?= True,
  "doesn't rhyme" ~: rhymes testWordPox testWordVision ~?= False
  ] where


test :: IO ()
test = do
  runTestTT (TestList [
    testRhymes,
    testAnalyze,
    testClassify
    ])
  return ()
