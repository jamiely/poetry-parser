module PoemParser(Token (TokWord, TokNewline), PoemParser.lex) where

import Test.HUnit
import PoemAnalyzer
import CMUPronouncingDictionary
import Data.Map (Map)
import qualified Data.Map as Map

data Token =
    TokWord Word -- a word
  | TokNewline -- a newline character
  deriving (Eq, Show)

-- Lexes a string into a list of tokens
lex :: [PoemLine] -> [Token]
lex _ = error "unimplemented"

testLex :: Test
testLex = TestList [
  True ~?= False
  ]

type RhymeStats = ([Phoneme], String)

type RhymeMap = Map String RhymeStats

newtype PoemParser = P ([Token] -> [(RhymeMap, [Token])])

doParse :: PoemParser -> [Token] -> [(RhymeMap, [Token])]
doParse (P p) pls = p pls

nSyllables :: Int -> PoemParser
nSyllables n = P (_syl n) where
  _syl n' ((TokWord word):ts) = 
    if sylCount < n' then _syl (n' - sylCount) ts
    else if sylCount == n' then [(Map.empty, ts)] 
    else [] where
      sylCount = syllables word

  _syl _ _                    = []

testTokenList1 :: [Token]
testTokenList1 = [
  TokWord testWordFox,
  TokWord testWordPox
  ]

testTokenList2 :: [Token]
testTokenList2 = [
  TokWord testWordTransfusion,
  TokWord testWordPox,
  TokWord testWordVision
  ]

testNSyllables :: Test
testNSyllables = "Test nSyllables" ~: TestList [
  doParse (nSyllables 2) testTokenList1 ~?= [(Map.empty, [])],
  doParse (nSyllables 2) [TokWord testWordTransfusion] ~?= []
  ]

test :: IO ()
test = do
  runTestTT (TestList [
    testNSyllables,
    testLex
    ])
  return ()
