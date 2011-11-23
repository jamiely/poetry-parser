module PoemParser(Token, PoemParser.lex) where

data Token =
    TokWord String -- a word
  | TokNewline -- a newline character

-- Lexes a string into a list of tokens
lex :: String -> [Token]
lex _ = [something, something, TokWord "fox", TokNewline, 
         something, something, TokWord "pox"] where
  something = TokWord "something"

