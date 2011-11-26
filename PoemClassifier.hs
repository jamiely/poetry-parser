module PoemClassifier (
  classify, getWords, rhymes, 
  PoemClassifier.test, Dictionary) where

import CMUPronouncingDictionary
import PoemParser
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split(splitOn)
import Data.Maybe

-- | Use to classify a poem. Given a poem, and dictionary, this will output information
-- about its type, rhyming scheme, and meter.
classify :: String -> String -> String
classify poem dictText = poemDescription where
  dict = loadWords dictText $ wordList poem
  words = justWords $ getWords (lines poem) dict
  poemDescription = analyze words

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

testClassify :: Test 
testClassify = "Test classify" ~: TestList [
  -- fails right now
  classify "something something fox\nsomething something pox" "SOMETHING  S AH1 M TH IH0 NG\nFOX  F AA1 K S\nPOX  P AA1 K S" ~?= "Rhyming poem aa"
  ]

testDictionary :: Dictionary
testDictionary = Map.fromList [
  ("fox", testWordFox), 
  ("pox", testWordPox)
  ]

-- | Given a list of tokens, returns an analysis of the poem pieces
getWords :: [String] -> Dictionary -> [[Maybe Word]]
getWords strs dict = map (map (getWord dict)) broken where
  broken = splitOn ["\n"] strs

testGetWords :: Test
testGetWords = "Test getWords" ~: TestList [
  "fox" ~: getWords ["fox"] testDictionary ~?= [[justFox]],
  "fox with breaks" ~: getWords ["fox", "\n", "fox", "fox"] testDictionary ~?= 
    [[justFox], [justFox, justFox]]
  ] where
  justFox = Just testWordFox

-- | Removes all Nothing's from the list and collapses entries
justWords :: [[Maybe Word]] -> [[Word]]
justWords w = filter (not . null) (map onlyWords w) where
  onlyWords :: [Maybe Word] -> [Word]
  onlyWords maybeWords = map fromJust (filter isJust maybeWords)

testJustWords :: Test
testJustWords = "Test justWords" ~: TestList [
  justWords [[Just testWordFox, Nothing], [Nothing, Nothing]] ~?= 
    [[testWordFox]]
  ]

-- | Returns statistics about a word given the word
getWord :: Dictionary -> String -> Maybe Word
getWord dict str = Map.lookup str dict

testGetWord :: Test
testGetWord = "Test getWord" ~: TestList [
  "just" ~: getWord testDictionary "fox" ~?= Just testWordFox,
  "nothing" ~: getWord testDictionary "nothing" ~?= Nothing
  ]

testWords :: [Word]
testWords = [testWordFox, testWordPox]


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
    testGetWord,
    testJustWords,
    testGetWords,
    testAnalyze,
    testClassify
    ])
  return ()
