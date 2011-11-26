module PoemParser(Token (TokWord, TokNewline), PoemParser.lex) where

import Test.HUnit

data Token =
    TokWord String -- a word
  | TokNewline -- a newline character
  deriving Eq

-- Lexes a string into a list of tokens
lex :: String -> [Token]
lex _ = [something, something, TokWord "fox", TokNewline, 
         something, something, TokWord "pox"] where
  something = TokWord "something"

testLex :: Test
testLex = TestList [
  True ~?= False
  ]

test :: IO ()
test = do
  runTestTT (TestList [
    testLex
    ])
  return ()
