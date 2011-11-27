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

newtype PoemParser a = P ([Token] -> [(a, [Token])])

doParse :: PoemParser a -> [Token] -> [(a, [Token])]
doParse (P p) pls = p pls

nSyllables :: Int -> PoemParser RhymeMap
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

-- | Used to parse line breaks
lineBreak :: PoemParser RhymeMap
lineBreak = P _linebr where 
  _linebr (TokNewline:ts) = [(Map.empty, ts)]
  _linebr _ = []

-- | Return, used in defining Monad instance of PoemParser
returnP :: a -> PoemParser a
returnP a = P (\cs -> [ (a, cs) ])

-- | >>=, used in defining Monad instance of PoemParser
bindP :: PoemParser a -> (a -> PoemParser b) -> PoemParser b
p1 `bindP` fp2 = P (\cs -> do 
  (a,cs') <- doParse p1 cs 
  doParse (fp2 a) cs'
  )

instance Monad PoemParser where
  (>>=)  = bindP
  return = returnP

testMonad :: Test
testMonad = "Test Monad PoemParser" ~: TestList [
  doParse (pair ns2 ns1) tokList  ~?= [(Map.empty, [])],
  doParse (pair ns1 ns2) tokList  ~?= []
  ] where
  ns2 = nSyllables 2
  ns1 = nSyllables 1
  tokList = [TokWord testWordVision, TokWord testWordFox]
  pair :: PoemParser a -> PoemParser b -> PoemParser RhymeMap
  pair a b = do
    x <- a
    y <- b
    return Map.empty

-- | Parses a Haiku
haiku :: PoemParser RhymeMap
haiku = do
  ns 5
  lineBreak
  ns 7
  lineBreak
  ns 5
  return Map.empty where
  ns = nSyllables

testHaiku :: Test
testHaiku = "Test haiku" ~: TestList [
  doParse haiku tokListGood1 ~?= [(Map.empty, [])],
  doParse haiku tokListGood2 ~?= [(Map.empty, [])],
  doParse haiku tokListBad1 ~?= [],
  doParse haiku tokListBad2 ~?= []
  ] where
  tokListGood1 = [f,f,f,f,f,br,f,f,f,f,f,f,f,br,f,f,f,f,f] 
  tokListGood2 = [t,v,br,t,v,v,br,t,v]
  tokListBad1 = [f,f,f,f,f,br,br] 
  tokListBad2 = [t,v,br,t,v,v,br,v,v]
  f = TokWord testWordFox -- 1 syllable each
  t = TokWord testWordTransfusion
  v = TokWord testWordVision
  br = TokNewline

test :: IO ()
test = do
  runTestTT (TestList [
    testNSyllables,
    testMonad,
    testHaiku,
    testLex
    ])
  return ()
