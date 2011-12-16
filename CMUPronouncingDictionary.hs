module CMUPronouncingDictionary (
  Phoneme, Word(Word), PoemLine, Dictionary,
  loadWords,
  testWordTransfusion, testWordPox,
  testWordVision, testWordFox,
  CMUPronouncingDictionary.dictTest,
  syllables, phonemes
) where

import Data.Char(toLower,toUpper)
import Data.List(isPrefixOf, isSuffixOf)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.Split(splitOn)
import Test.HUnit

type Phoneme = String

data Word =
    Word String Int String [Phoneme] -- actual word, syllables, stress, phonemes
    deriving (Eq, Show)

type PoemLine = [Word]

type Dictionary = Map String Word

-- | Generates a dictionary based on the passed words
loadWords :: String -> [String] -> Dictionary
loadWords strDict strWords = Map.fromList wordList where
  relevantDictionaryLines = filter (/= "") $ map findWordLine strWords
  dictLines = lines strDict
  -- | Returns "" if the word is not found
  findWordLine w = if null ws then "" else head ws where
    ws = findWordLines w
  findWordLines w = filter (isPrefixOf ((map toUpper w)++" ")) dictLines
  wordList = map wordTuple relevantDictionaryLines
  wordTuple line = (strWord word, word) where
    word = dictLineToWord line
    strWord (Word text _ _ _) = text

testLoadWords :: Test 
testLoadWords = "Test loadWords" ~: TestList[
  loadWords testCorpus ["fox", "pox", "notindictionary"] ~?=
    Map.fromList [("fox", testWordFox), ("pox", testWordPox)]
  ]

dictLineToWord :: String -> Word
dictLineToWord line = Word strWord syllableCount stress phonemes where
  (strWord, phonemes) = case (words line) of 
    [] -> ("", [])
    (x:xs) -> (x, xs)
  stress = stressPattern phonemes
  stressPhonemes = filter isStressPhoneme phonemes
  syllableCount = length stressPhonemes 

testDictLineToWord :: Test
testDictLineToWord = "Test dictLineToWord" ~: TestList [
  dictLineToWord "FOX  F AA1 K S" ~?= testWordFox 
  ]

testWordFox :: Word
testWordFox = Word "fox" 1 "^" ["F", "AA1", "K", "S"]

testWordPox :: Word
testWordPox = Word "pox" 1 "^" ["P", "AA1", "K", "S"]

testWordVision :: Word
testWordVision = Word "vision" 2 "^_" ["V", "IH1", "ZH", "AH0", "N"]

testWordTransfusion :: Word
testWordTransfusion = Word "transfusion" 3 "_^_" ["T", "R", "AE0", "N", "S", "F", "Y", "UW1", "ZH", "AH0", "N"]

testCorpus :: String
testCorpus = "FOX  F AA1 K S"
  ++ "\nPOX  P AA1 K S"
  ++ "\nSOX  S AA1 K S"
  ++ "\nVISION  V IH1 ZH AH0 N"
  ++ "\nTRANSFUSION  T R AE0 N S F Y UW1 ZH AH0 N"
  ++ "\nSOMETHING  S AH1 M TH IH0 NG"

-- | Use to determine the number of syllables in a word
syllables :: Word -> Int
syllables (Word _ syl _ _ ) = syl

-- | Use to retrieve the phonemes associated with a word
phonemes :: Word -> [Phoneme]
phonemes (Word _ _ _ p) = p

testSyllables :: Test
testSyllables = "Test syllables" ~: TestList [
  "transfusion" ~: syllables testWordTransfusion ~?= 3,
  "vision" ~: syllables testWordVision ~?= 2,
  "pox" ~: syllables testWordPox ~?= 1
  ]

-- | Returns the pattern of stresses. For example, a word with a
-- stressed, then two unstressed symbols will return string "^__"
-- @todo consider replacing with list of bools, or new datatype
stressPattern :: [Phoneme] -> String
stressPattern []     = ""
stressPattern (p:ps) = pat ++ stressPattern ps where
  pat = case lst of
    '1' -> "^"
    '2' -> "^"
    '0' -> "_"
    _   -> ""
  lst = if null pat then '\0' else last pat

testStressPattern :: Test
testStressPattern = "Test stressPattern" ~: TestList [
  stressPattern ["A1", "A0", "AB", "A1"] ~?= "^_^"
  ]

-- | Use to retrieve the stressed phonemes in a word
stresses :: Word -> [Phoneme]
stresses (Word _ _ _ ph) = filter isStressPhoneme ph

testStresses :: Test
testStresses = "Test stresses" ~: TestList [
  "transfusion" ~: stresses testWordTransfusion ~?= ["AE0", "UW1", "AH0"]
  ]

-- | Phonemes which end in 0, 1, or 2 are syllables
isStressPhoneme :: Phoneme -> Bool
isStressPhoneme s = any (\n -> isSuffixOf n s) ["0", "1", "2"]

testIsStressPhoneme :: Test
testIsStressPhoneme = "Test isStressPhoneme" ~: TestList [
  isStressPhoneme "A" ~?= False,
  isStressPhoneme "A1" ~?= True,
  isStressPhoneme "A3" ~?= False,
  isStressPhoneme "VERYLONG1" ~?= True
  ]

dictTest :: IO ()
dictTest = do
  runTestTT ( TestList [
    testSyllables,
    testStresses,
    testStressPattern,
    testIsStressPhoneme,
    testDictLineToWord,
    testLoadWords
    ] )
  return ()
