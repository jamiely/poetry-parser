module PoemParser(Token (TokLine), PoemParser.lex, haiku, aba, aabba, limerick, limerickStress,
  iambicPentameter, sonnetRhyme, shakespeareanSonnet, 
 PoemParser, RhymeMap, doParse) where

import Test.HUnit
import PoemAnalyzer
import CMUPronouncingDictionary
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAlpha)
import Control.Monad.State

data Token = TokLine Line --the fundamental unit of a poem shall be a line
  deriving (Eq, Show)

--Original idea:
--data Token =
--    TokWord Word -- a word
--  | TokNewline -- a newline character
--  deriving (Eq, Show)

type RhymeMap = Map [Phoneme] String 

--Originally: 
--type RhymeMap = Map String RhymeStats
--type RhymeStats = ([Phoneme], String)

newtype PoemParser a = P ([Token] -> [(a, [Token])])

-- Lexes a string into a list of tokens
lex :: [PoemLine] -> [Token]
lex = map (TokLine . wordsToLine)

testLex :: Test
testLex = "testLex" ~: TestList [
  PoemParser.lex [[testWordFox, testWordPox], 
      [testWordVision, testWordTransfusion]] ~?= 
    [TokLine testLineFoxPox, TokLine testLineVisionTransfusion] 
  ]

-- | Apply a parser to a list of tokens
doParse :: PoemParser a -> [Token] -> [(a, [Token])]
doParse (P p) pls = p pls

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
  tokList = map TokLine [testLineVision, testLineFox]

-- Functions used in parsing 

lettersOnly :: [String] -> [String]
lettersOnly = map (filter isAlpha)

-- | Returns the final vowel sound of a word followed by any 
-- consonants that come after it.
lastVowelPhonemes :: Line -> [Phoneme]
lastVowelPhonemes (Line _ _ _ phs) = acc (reverse phs) [] where
  acc (c:cs) phs' = if   isStressPhoneme c
                    then c:phs'
                    else acc cs (c:phs')
  acc [] _ = []
               
testLastVowelPhonemes :: Test
testLastVowelPhonemes = "lastVowelPhonemes" ~: TestList $
  zipWith (~?=) (map lastVowelPhonemes [testLineFox, testLineTransfusion])
                    [ ["AA1", "K", "S"], ["AH0", "N"]]

--Old way of testing rhyming

---- | Returns the last n phonemes of a line, or all of them if 
---- the total is less than n.
--lastPhonemes :: Int -> Line -> [Phoneme]
--lastPhonemes n (Line _ _ _ phs)  
--  | length phs <= n = phs
--  | otherwise       = drop ((length phs) - n) phs 
--
--testLastPhonemes :: Test
--testLastPhonemes = let testWords = [testLineFoxPox, testLineVisionTransfusion] 
--  in "lastPhonemes" ~: 
--  TestList (zipWith (~?=) (map (lastPhonemes 3) testWords) 
--                          [["AA1", "K", "S"], ["ZH", "AH0", "N"]]) 
--
--last3Phonemes :: Line -> [Phoneme]
--last3Phonemes = lastPhonemes 3

---- | Use to determine if two lines rhyme. Compares the last vowel phonemes
---- of each line to determine if they rhyme.
--rhymes :: Line -> Line -> Bool
--rhymes l1 l2  = (fun l1) == (fun l2) where
--  fun = lettersOnly . lastVowelPhonemes 
--
--testRhymes :: Test
--testRhymes = "Test Rhymes" ~: TestList [
--  "-zion" ~: rhymes tfsn vsn ~?= True, 
--  "-ox" ~: rhymes fx px ~?= True,
--  "doesn't rhyme" ~: rhymes testLineFoxPox testLineVisionTransfusion ~?= False
--  ] where 
--    (tfsn, vsn) = (testLineTransfusion, testLineVision) 
--    (fx, px) = (testLineFox, testLinePox)

--  Fundamental parsers: syllables, rhyme, and meter

-- | Creates a parser for a line with a given number of syllables
nSyllables :: Int -> PoemParser RhymeMap
nSyllables n = P (_syl n) where
  _syl n' ((TokLine (Line _ sylCount _ _):ts)) 
    |  n' == sylCount = [(Map.empty, ts)]
    | otherwise       = []
  _syl _ _                    = []

testNSyllables :: Test
testNSyllables = "Test nSyllables" ~: TestList [
  doParse (nSyllables 2) testTokenList1 ~?= 
    [(Map.empty, [TokLine testLineVisionTransfusion])],
  doParse (nSyllables 2) [TokLine (testLineTransfusion)] ~?= []
  ]

-- | Makes a parser that succeeds when the given line
-- receives the given string as its key in the map, 
-- representing a piece of a rhyme scheme.
thisRhyme :: String -> RhymeMap -> PoemParser RhymeMap
thisRhyme str m = P parse where
  parse ((TokLine l):ls) = let phons = lettersOnly $ lastVowelPhonemes l in 
    case (phons `Map.lookup` m) of
      Just str' -> if str == str' 
                   then [(m, ls)] 
                   else []
      Nothing -> if str == nk 
                 then [(Map.insert phons nk m, ls)]
                 else []  
  parse _ = []  
  ks = Map.keys m
  nk = nextKey ks
  nextKey ks' = infi !! (length ks')
  infi = abcs ++ (concatMap (\x -> map (x ++) abcs) infi) where
    abcs = map (:[]) ['a' .. 'z'] 

-- functionality further demonstrated by working testAba,
-- which depends on thisRhyme
testThisRhyme :: Test
testThisRhyme = "thisRhyme" ~: TestList [
  doParse (thisRhyme "a" Map.empty) t ~?= [(map, [])]
    ] where
    t = [TokLine testLineFoxPox]
    map = Map.fromList [(["AA", "K", "S"], "a")]

-- | Takes a stress pattern and gives a parser for lines of 
-- that pattern.
stressLine :: [Stress] -> PoemParser RhymeMap
stressLine sts = P parse where
  parse [] = []
  parse ((TokLine (Line _ _ sts' _)):ls) = if sts == sts'
                                 then [(Map.empty, ls)]
                                 else [] 

testStressLine :: Test
testStressLine = "stressLine" ~: TestList $
  zipWith (~?=) list [[(Map.empty, [])], []] where
    list = [doParse (stressLine [U]) [TokLine testLineFox],
            doParse (stressLine [D]) [TokLine testLineFox] ]

-- | Parses any line 
anyLine :: PoemParser RhymeMap
anyLine = P parse where
  parse ((TokLine _):ts) = [(Map.empty, ts)]
  parse _                = []

testAnyLine :: Test
testAnyLine = "Test anyLine" ~: TestList [
  doParse anyLine tokList ~?= [(Map.empty, tail tokList)],
  doParse anyLine [] ~?= []
  ] where
  tokList = [TokLine testLineVisionTransfusion, TokLine testLineFoxPox]

-- Parser combinators 

-- | Tries two parsers and combines their successes.
chooseP :: PoemParser a -> PoemParser a -> PoemParser a
p1 `chooseP` p2 = P (\cs -> let ls1 = doParse p1 cs in   --ls1 :: [(a,String)]
                            let ls2 = doParse p2 cs in   --ls2 :: [(a,String)]
                            ls1 ++ ls2)

-- | Short circuit. Tries the first parser. If that fails, tries the second.
(<|>) :: PoemParser a -> PoemParser a -> PoemParser a
p1 <|> p2 = P (\ts -> case doParse p1 ts of
                        []  -> doParse p2 ts 
                        win -> win)

-- | Takes a parser and applies it till it can't be
-- applied no more. If all of the input has been consumed,
-- succeed.
whileParse :: PoemParser a -> PoemParser a
whileParse p = P parse where
  parse [] = []
  parse ls = case doParse p ls of
    s@[(_,[])] -> s --success, no more tokens 
    [(_,ls')] -> parse ls' --keep going
    _ -> [] 

-- | Takes two parsers and applies them to the same input.
-- Only succeeds if both succeed. Only the output of the second 
-- parser is kept.
(<&>) :: PoemParser a -> PoemParser b -> PoemParser b
p1 <&> p2 = P parse where
  parse [] = []
  parse ls = case doParse p1 ls of
    [_] -> doParse p2 ls 
    _         -> []

-- | Does two parsers in succession, throwing away the result.
pair :: PoemParser a -> PoemParser b -> PoemParser RhymeMap
pair a b = do
  x <- a
  y <- b
  return Map.empty

-- Apply two parsers in succession, starting with a seed store
-- and returning the final result.
pairSeed :: a -> (a -> PoemParser a) -> (a -> PoemParser a) -> PoemParser a
pairSeed seed pf1 pf2 = do
  seed'  <- (pf1 seed)
  seed'' <- (pf2 seed')
  return seed''

-- Poetic forms built out of the preceding

-- | Parses a Haiku
haiku :: PoemParser RhymeMap
haiku = do
  ns 5
  ns 7
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
  tokListGood1 = [f5, f7, f5] 
  tokListGood2 = [tv, tvv, tv] 
  tokListBad1  =  [f5] 
  tokListBad2  =  [f5, tvv, vv]
  f5  = TokLine $ wordsToLine $ replicate 5 f 
  f7  = TokLine $ wordsToLine $ replicate 7 f
  tv  = TokLine $ wordsToLine $ [t, v]
  tvv = TokLine $ wordsToLine $ [t, v, v]
  vv  = TokLine $ wordsToLine $ [v, v]
  f  = testWordFox -- 1 syllable each
  t  = testWordTransfusion
  v  = testWordVision

-- | Takes a string representing a rhyme scheme and a 
-- RhymeMap and outputs a parser for poems of that scheme.
rhymeScheme :: String -> RhymeMap -> PoemParser RhymeMap
rhymeScheme [] m = P (\toks -> [(m, toks)])
rhymeScheme (c:cs) m = pairSeed m (thisRhyme [c]) (rhymeScheme cs)

-- | Parses a 3-line poem with an aba rhyme scheme
aba :: PoemParser RhymeMap
aba = rhymeScheme "aba" Map.empty 

testAba :: Test
testAba = "ABA" ~: TestList $
  zipWith (~?=) (map (doParse aba) [goodPoem, badPoem])
                                    [[(m, [])], []] where
    goodPoem = poemize [[f],[v],[p]]
    badPoem  = poemize [[v],[f],[p]]
    poemize = map (TokLine . wordsToLine)
    m = Map.fromList [(["AA", "K", "S"], "a"), (["AH", "N"], "b")]
    f = testWordFox
    v = testWordVision
    p = testWordPox

aabba :: PoemParser RhymeMap
aabba = rhymeScheme "aabba" Map.empty 

-- | Just for the heck of it
rhymingHaiku :: PoemParser RhymeMap
rhymingHaiku = haiku <&> aba 

-- | The stress pattern of a limerick
-- note: syllable requirement is implicit
limerickStress :: PoemParser RhymeMap
limerickStress = do
                 stressLine [D,U, D,D,U, D,D,U] 
                 stressLine [D,U, D,D,U, D,D,U] 
                 stressLine [D,U, D,D,U]        
                 stressLine [D,U, D,D,U]        
                 stressLine [D,U, D,D,U, D,D,U] 

limerick :: PoemParser RhymeMap
limerick = limerickStress <&> aabba 

sonnetRhyme :: PoemParser RhymeMap
sonnetRhyme = rhymeScheme "ababcdcdefefgg" Map.empty

iambicPentameter :: PoemParser RhymeMap
iambicPentameter = whileParse sp where
  -- | Allow for inversion of first or fourth foot, and feminine ending. 
  sp = stressLine [D,U, D,U, D,U, D,U, D,U] <|>
       stressLine [D,U, D,U, D,U, D,U, D,U] <|>
       stressLine [U,D, D,U, D,U, D,U, D,U] <|>
       stressLine [D,U, D,U, D,U, U,D, D,U] <|>
       stressLine [U,D, D,U, D,U, U,D, D,U,D] <|>
       stressLine [U,D, D,U, D,U, D,U, D,U,D] <|>
       stressLine [D,U, D,U, D,U, U,D, D,U,D] <|>
       stressLine [U,D, D,U, D,U, U,D, D,U,D]

-- | The creme de la creme 
shakespeareanSonnet :: PoemParser RhymeMap
shakespeareanSonnet = iambicPentameter <&>  
                       (rhymeScheme "ababcdcdefefgg" Map.empty)

testTokenList1 :: [Token]
testTokenList1 = [
  TokLine testLineFoxPox,
  TokLine testLineVisionTransfusion
  ]

test :: IO ()
test = do
  runTestTT (TestList [
    testLastVowelPhonemes, 
    testAnyLine,
    testStressLine,  
    testNSyllables,
    testMonad,
    testHaiku,
    testAba,
    testLex
    ])
  return ()
