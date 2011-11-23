module PoemClassifier (
  classify, analyze, rhymes, 
  syllables, stresses, Word, 
  PoemClassifier.test) where

import PoemParser
import Data.List (isSuffixOf)
import Test.HUnit

-- | Use to classify a poem. Given a poem, and dictionary, this will output information
-- about its type, rhyming scheme, and meter.
classify :: String -> String -> String
classify _ _ = error "not implemented"

testClassify :: Test 
testClassify = "Test classify" ~: TestList [
  -- fails right now
  classify "something something fox\nsomething something pox" "SOMETHING  S AH1 M TH IH0 NG\nFOX  F AA1 K S\nPOX  P AA1 K S" ~?= "Rhyming poem aa"
  ]

type Phoneme = String

data Word =
    Word String Int String [Phoneme] -- actual word, syllables, stress, phonemes
    deriving (Eq, Show)

-- | Given a list of tokens, returns an analysis of the poem pieces
analyze :: [Token] -> [[Word]]
analyze _ = error "not implemented"

testAnalyze :: Test
testAnalyze = "Test analyze" ~: TestList [
  "fox" ~: analyze [TokWord "fox"] ~?= [[testWordFox]]
  ]

testWords :: [Word]
testWords = [testWordFox, testWordPox]

testWordFox :: Word
testWordFox = Word "fox" 1 "^" ["F", "AA1", "K", "S"]

testWordPox :: Word
testWordPox = Word "pox" 1 "^" ["P", "AA1", "K", "S"]

testWordVision :: Word
testWordVision = Word "vision" 2 "^_" ["V", "IH1", "ZH", "AH0", "N"]

testWordTransfusion :: Word
testWordTransfusion = Word "transfusion" 3 "_^_" ["T", "R", "AE0", "N", "S", "F", "Y", "UW1", "ZH", "AH0", "N"]

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

-- | Use to determine the number of syllables in a word
syllables :: Word -> Int
syllables = length . stresses 

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
  lst = head $ reverse p -- @todo, bug here

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


test :: IO ()
test = do
  runTestTT (TestList [
    testRhymes,
    testSyllables,
    testStresses,
    testStressPattern,
    testIsStressPhoneme,
    testAnalyze,
    testClassify
    ])
  return ()
