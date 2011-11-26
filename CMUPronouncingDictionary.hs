module CMUPronouncingDictionary where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.Split(splitOn)

type Phoneme = String

data Word =
    Word String Int String [Phoneme] -- actual word, syllables, stress, phonemes
    deriving (Eq, Show)

type PoemWords = [[Word]]

type Dictionary = Map String Word

-- | Generates a dictionary based on the passed words
loadWords :: String -> [String] -> Dictionary
loadWords strDict strWords = Map.fromList wordList where
  relevantDictionaryLines = map findWordLine strWords
  dictLines = lines strDict
  findWordLine w = if null ws then "" else head ws where
    ws = findWordLines w
  findWordLines w = filter (isPrefixOf (w++" ")) dictLines

testLoadWords :: Test 
testLoadWords = "Test loadWords" ~: TestList[
  loadWords testCorpus ["fox", "pox", "notindictionary"] ~?=
    Map.fromList [("fox", testWordFox), ("pox", testWordPox)]
  ]

testWordFox :: Word
testWordFox = Word "fox" 1 "^" ["F", "AA1", "K", "S"]

testWordPox :: Word
testWordPox = Word "pox" 1 "^" ["P", "AA1", "K", "S"]

testCorpus :: String
testCorpus = "FOX  F AA1 K S"
  ++ "\nPOX  P AA1 K S"
  ++ "\nSOX  S AA1 K S"
  ++ "\nVISION  V IH1 ZH AH0 N"
  ++ "\nTRANSFUSION  T R AE0 N S F Y UW1 ZH AH0 N"
  ++ "\nSOMETHING  S AH1 M TH IH0 NG"



