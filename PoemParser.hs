module PoemParser(Token (TokLine), PoemParser.lex, haiku,
 PoemParser, RhymeMap, doParse) where

import Test.HUnit
import PoemAnalyzer
import CMUPronouncingDictionary
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAlpha)
import Control.Monad.State

data Token = TokLine Line
--    TokWord Word -- a word
--  | TokNewline -- a newline character
  deriving (Eq, Show)

-- Lexes a string into a list of tokens
lex :: [PoemLine] -> [Token]
lex = map (TokLine . wordsToLine)

testLex :: Test
testLex = TestList [
  PoemParser.lex [[testWordFox, testWordPox], [testWordVision, testWordTransfusion]] ~?= 
    [TokLine testLineFoxPox, TokLine testLineVisionTransfusion] 
  ]

type RhymeMap = Map [Phoneme] String 

newtype PoemParser a = P ([Token] -> [(a, [Token])])

doParse :: PoemParser a -> [Token] -> [(a, [Token])]
doParse (P p) pls = p pls

lastPhonemes :: Int -> Line -> [Phoneme]
lastPhonemes n (Line _ _ _ phs)  
  | length phs <= n = phs
  | otherwise       = drop ((length phs) - n) phs 

testLastPhonemes :: Test
testLastPhonemes = let testWords = [testLineFoxPox, testLineVisionTransfusion] in
  "lastPhonemes" ~: 
  TestList (zipWith (~?=) (map (lastPhonemes 3) testWords) 
                          [["AA1", "K", "S"], ["ZH", "AH0", "N"]]) 

last3Phonemes :: Line -> [Phoneme]
last3Phonemes = lastPhonemes 3

-- | If the first item in the input stream is the last word
-- on a line, parses a rhyme map from that word.
lastWord :: RhymeMap -> PoemParser RhymeMap
lastWord m = P fun where
  fun ((TokLine l):ls) = [(m', ls)] where
    m' = if phons `Map.member` m
         then m
         else Map.insert phons nk m
    phons = lettersOnly $ last3Phonemes l 
    ks = Map.keys m
    nk = nextKey ks
  fun _ = []
  nextKey ks' = infi !! (length ks')
  infi = abcs ++ (concatMap (\x -> map (x ++) abcs) infi) where
    abcs = map (:[]) ['a' .. 'z'] 

testLastWord :: Test
testLastWord = "lastWord" ~: TestList [
  doParse (lastWord Map.empty) t ~?= [(map, [])]
  ] where
  t = [TokLine testLineFoxPox]
  map = Map.fromList [(["AA", "K", "S"], "a")]
 
lettersOnly :: [String] -> [String]
lettersOnly = map (filter isAlpha)

-- | Returns true if the last n phonemes in the passed lists match
phonemesMatch :: Int -> [Phoneme] -> [Phoneme] -> Bool
phonemesMatch n a b = lettersOnly (part a) == lettersOnly (part b) where
  part = (take n) . reverse

testPhonemesMatch :: Test
testPhonemesMatch = "Test phonemesMatch" ~: TestList [
  phonemesMatch 3 ["a", "b", "c"] ["x", "x", "a", "b", "c"] ~?= True
  ]

-- | Parses any line 
anyLine :: PoemParser RhymeMap
anyLine = P fun where
  fun ((TokLine _):ts) = [(Map.empty, ts)]
  fun _                = []

testAnyLine :: Test
testAnyLine = "Test anyLine" ~: TestList [
  doParse anyLine tokList ~?= [(Map.empty, tail tokList)],
  doParse anyLine [] ~?= []
  ] where
  tokList = [TokLine testLineVisionTransfusion, TokLine testLineFoxPox]

chooseP :: PoemParser a -> PoemParser a -> PoemParser a
p1 `chooseP` p2 = P (\cs -> let ls1 = doParse p1 cs in   --ls1 :: [(a,String)]
                            let ls2 = doParse p2 cs in   --ls2 :: [(a,String)]
                            ls1 ++ ls2)

-- | Use to determine if two lines rhyme. Compares the last 3 phonemes
-- of each line to determine if they rhyme.
rhymes :: Line -> Line -> Bool
rhymes (Line _ _ _ ph1) (Line _ _ _ ph2) = phonemesMatch 3 ph1 ph2

testRhymes :: Test
testRhymes = "Test Rhymes" ~: TestList [
  "-zion" ~: rhymes tfsn vsn ~?= True, 
  "-ox" ~: rhymes fx px ~?= True,
  "doesn't rhyme" ~: rhymes testLineFoxPox testLineVisionTransfusion ~?= False
  ] where 
    (tfsn, vsn) = (testLineTransfusion, testLineVision) 
    (fx, px) = (testLineFox, testLinePox)

-- | Takes a string and a RhymeMap and gives a parser which
-- (a) rhymes with something in the map
-- (b) the thing it rhymes with is tagged with the corresonding pattern
rhymesPattern :: String -> RhymeMap -> PoemParser RhymeMap
rhymesPattern pat m = P fun where
  fun ((TokLine l):ts) = let phons = lettersOnly $ last3Phonemes l in
    case phons `Map.lookup` m of
      Just s -> if   (pat == s)
                then [(m, ts)]
                else []
      Nothing -> []
  fun _ = []

testRhymesPattern :: Test
testRhymesPattern = TestList $ 
  zipWith (~?=) (map (doParse par . (:[]) . TokLine ) [testLinePox, testLineVision]) 
                  [[(m, [])], []] where
    par = rhymesPattern "a" m
    m = fst . head $ doParse (lastWord Map.empty) $ [TokLine testLineFox] 

-- | Takes a RhymeMap and gives a parser which consumes input
-- that rhymes with something in that map.
rhymeIn :: RhymeMap -> PoemParser RhymeMap 
rhymeIn m = P fun where
  fun ((TokLine l):ts) = if phons `Map.member` m
                            then [(m, ts)] 
                            else [] where 
    phons = lettersOnly $ last3Phonemes l
  fun _                   = []

testRhymeIn :: Test
testRhymeIn = "Test rhymeIn" ~: TestList [
  doParse r [fox, vision] ~?= [(rhymeMap, [vision])],
  doParse r [vision, fox] ~?= []
  ] where
  r = rhymeIn rhymeMap
  fox = TokLine testLineFox
  vision = TokLine testLineVision
  rhymeMap = Map.fromList [(["AA", "K", "S"], "a")]

-- | Parses a 3-line poem with an aba rhyme scheme
aba :: PoemParser RhymeMap
aba = do
  m <- lastWord Map.empty
  m' <- lastWord m
  rhymesPattern "a" m'

testAba :: Test
testAba =  TestList $
  zipWith (~?=) (map (doParse aba) [goodPoem, badPoem])
                                    [[(m, [])], []] where
    goodPoem = poemize [[f],[v],[p]]
    badPoem  = poemize [[v],[f],[p]]
    poemize = map (TokLine . wordsToLine)
    m = Map.fromList [(["AA", "K", "S"], "a"), (["ZH", "AH", "N"], "b")]
    f = testWordFox
    v = testWordVision
    p = testWordPox


nSyllables :: Int -> PoemParser RhymeMap
nSyllables n = P (_syl n) where
  _syl n' ((TokLine (Line _ sylCount _ _):ts)) 
    |  n' == sylCount = [(Map.empty, ts)]
    | otherwise       = []
  _syl _ _                    = []

testTokenList1 :: [Token]
testTokenList1 = [
  TokLine testLineFoxPox,
  TokLine testLineVisionTransfusion
  ]

testTokenList2 :: [Token]
testTokenList2 = [
  TokLine testLineTransfusion,
  TokLine testLinePox,
  TokLine testLineVision
  ]

testNSyllables :: Test
testNSyllables = "Test nSyllables" ~: TestList [
  doParse (nSyllables 2) testTokenList1 ~?= 
    [(Map.empty, [TokLine testLineVisionTransfusion])],
  doParse (nSyllables 2) [TokLine (testLineTransfusion)] ~?= []
  ]

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
  pair :: PoemParser a -> PoemParser b -> PoemParser RhymeMap
  pair a b = do
    x <- a
    y <- b
    return Map.empty

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

test :: IO ()
test = do
  runTestTT (TestList [
    testLastPhonemes, 
    testPhonemesMatch,
    testLastWord,
    testRhymes,
    testRhymesPattern,
    testRhymeIn,
    testNSyllables,
    testMonad,
    testHaiku,
    testAba,
    testLex
    ])
  return ()
