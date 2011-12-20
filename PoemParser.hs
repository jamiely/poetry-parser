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

data Token = TokLine Line
  deriving (Eq, Show)

-- Lexes a string into a list of tokens
lex :: [PoemLine] -> [Token]
lex = map (TokLine . wordsToLine)

testLex :: Test
testLex = "testLex" ~: TestList [
  PoemParser.lex [[testWordFox, testWordPox], 
      [testWordVision, testWordTransfusion]] ~?= 
    [TokLine testLineFoxPox, TokLine testLineVisionTransfusion] 
  ]

type RhymeMap = Map [Phoneme] String 

newtype PoemParser a = P ([Token] -> [(a, [Token])])

doParse :: PoemParser a -> [Token] -> [(a, [Token])]
doParse (P p) pls = p pls


-- | Returns the final vowel sound of a word followed by any 
-- consonants that come after it.
lastVowelPhonemes :: Line -> [Phoneme]
lastVowelPhonemes (Line _ _ _ phs) = fun (reverse phs) [] where
  fun (c:cs) phs' = if   isStressPhoneme c
                    then c:phs'
                    else fun cs (c:phs')
  fun [] _ = []
               
testLastVowelPhonemes :: Test
testLastVowelPhonemes = "lastVowelPhonemes" ~: TestList $
  zipWith (~?=) (map lastVowelPhonemes [testLineFox, testLineTransfusion])
                    [ ["AA1", "K", "S"], ["AH0", "N"]]

-- | Returns the last n phonemes of a line, or all of them if 
-- the total is less than n.
lastPhonemes :: Int -> Line -> [Phoneme]
lastPhonemes n (Line _ _ _ phs)  
  | length phs <= n = phs
  | otherwise       = drop ((length phs) - n) phs 

testLastPhonemes :: Test
testLastPhonemes = let testWords = [testLineFoxPox, testLineVisionTransfusion] 
  in "lastPhonemes" ~: 
  TestList (zipWith (~?=) (map (lastPhonemes 3) testWords) 
                          [["AA1", "K", "S"], ["ZH", "AH0", "N"]]) 

last3Phonemes :: Line -> [Phoneme]
last3Phonemes = lastPhonemes 3

-- | If the first item in the input stream is the last word
-- on a line, parses a rhyme map from that word.
thisRhyme :: String -> RhymeMap -> PoemParser RhymeMap
thisRhyme str m = P fun where
  fun ((TokLine l):ls) = let phons = lettersOnly $ lastVowelPhonemes l in 
    case (phons `Map.lookup` m) of
      Just str' -> if str == str' 
                   then [(m, ls)] 
                   else []
      Nothing -> if str == nk 
                 then [(Map.insert phons nk m, ls)]
                 else []  
  fun _ = []  
  ks = Map.keys m
  nk = nextKey ks
  nextKey ks' = infi !! (length ks')
  infi = abcs ++ (concatMap (\x -> map (x ++) abcs) infi) where
    abcs = map (:[]) ['a' .. 'z'] 

-- | If the first item in the input stream is the last word
-- on a line, parses a rhyme map from that word.
lastWord :: RhymeMap -> PoemParser RhymeMap
lastWord m = P fun where
  fun ((TokLine l):ls) = [(m', ls)] where
    m' = if phons `Map.member` m
         then m
         else Map.insert phons nk m
    phons = lettersOnly $ lastVowelPhonemes l 
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

-- | Use to determine if two lines rhyme. Compares the last vowel phonemes
-- of each line to determine if they rhyme.
rhymes :: Line -> Line -> Bool
rhymes l1 l2  = (fun l1) == (fun l2) where
  fun = lettersOnly . lastVowelPhonemes 

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
  fun ((TokLine l):ts) = let phons = lettersOnly $ lastVowelPhonemes l in
    case phons `Map.lookup` m of
      Just s -> if   (pat == s)
                then [(m, ts)]
                else []
      Nothing -> []
  fun _ = []

testRhymesPattern :: Test
testRhymesPattern = TestList $ 
  zipWith (~?=) (map (doParse par . (:[]) . TokLine ) 
                  [testLinePox, testLineVision]) [[(m, [])], []] where
    par = rhymesPattern "a" m
    m = fst . head $ doParse (lastWord Map.empty) $ [TokLine testLineFox] 

-- | Takes a RhymeMap and gives a parser which consumes input
-- that rhymes with something in that map.
rhymeIn :: RhymeMap -> PoemParser RhymeMap 
rhymeIn m = P fun where
  fun ((TokLine l):ts) = if phons `Map.member` m
                            then [(m, ts)] 
                            else [] where 
    phons = lettersOnly $ lastVowelPhonemes l
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

-- | Takes a parser and applies it till it can't be
-- applied no more. If all of the input has been consumed,
-- succeed.
whileParse :: PoemParser a -> PoemParser a
whileParse p = P fun where
  fun [] = []
  fun ls = case doParse p ls of
    s@[(_,[])] -> s 
    [(_,ls')] -> fun ls'
    _ -> [] 

-- | Tries the first parser. If that fails, tries the second.
(<|>) :: PoemParser a -> PoemParser a -> PoemParser a
p1 <|> p2 = P (\ts -> case doParse p1 ts of
                        []  -> doParse p2 ts 
                        win -> win)

-- | Takes two parsers and applies them to the same input.
-- Only succeeds if both succeed. Only the output of the second 
-- parser is kept.
andParse :: PoemParser a -> PoemParser b -> PoemParser b
andParse p1 p2 = P fun where
  fun [] = []
  fun ls = case doParse p1 ls of
    [_] -> doParse p2 ls 
    _         -> []

rhymingHaiku :: PoemParser RhymeMap
rhymingHaiku = andParse haiku aba 

limerickStress :: PoemParser RhymeMap
limerickStress = do
                 stressLine [D,U, D,D,U, D,D,U] 
                 stressLine [D,U, D,D,U, D,D,U] 
                 stressLine [D,U, D,D,U]        
                 stressLine [D,U, D,D,U]        
                 stressLine [D,U, D,D,U, D,D,U] 

limerick :: PoemParser RhymeMap
limerick = limerickStress `andParse` rhymeScheme "aabba" Map.empty

sonnetRhyme :: PoemParser RhymeMap
sonnetRhyme = rhymeScheme "ababcdcdefefgg" Map.empty

shakespeareanSonnet :: PoemParser RhymeMap
shakespeareanSonnet = iambicPentameter `andParse`  
                       (rhymeScheme "ababcdcdefefgg" Map.empty)

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

-- | Takes a stress pattern and gives a parser for lines of 
-- that pattern.
stressLine :: [Stress] -> PoemParser RhymeMap
stressLine sts = P fun where
  fun [] = []
  fun ((TokLine (Line _ _ sts' _)):ls) = if sts == sts'
                                 then [(Map.empty, ls)]
                                 else [] 

testStressLine :: Test
testStressLine = "stressLine" ~: TestList $
  zipWith (~?=) list [[(Map.empty, [])], []] where
    list = [doParse (stressLine [U]) [TokLine testLineFox],
            doParse (stressLine [D]) [TokLine testLineFox] ]



-- | Takes a string representing a rhyme scheme and a 
-- RhymeMap and outputs a parser for poems of that scheme.
rhymeScheme :: String -> RhymeMap -> PoemParser RhymeMap
rhymeScheme [] m = P (\toks -> [(m, toks)])
rhymeScheme (c:cs) m = pairSeed m (thisRhyme [c]) (rhymeScheme cs)

-- | Parses a 3-line poem with an aba rhyme scheme
aba :: PoemParser RhymeMap
aba = rhymeScheme "aba" Map.empty 

-- | Parses a 5-line poem with an aabba rhyme scheme
aabba :: PoemParser RhymeMap
aabba = rhymeScheme "aabba" Map.empty 

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

-- | Makes a parser out of a seed and two functions
-- needing seeds and producing parsers. The resulting parser
-- will apply both in succession.
pairSeed :: a -> (a -> PoemParser a) -> (a -> PoemParser a) -> PoemParser a
pairSeed seed pf1 pf2 = do
  seed'  <- (pf1 seed)
  seed'' <- (pf2 seed')
  return seed''

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
    testLastVowelPhonemes, 
    testLastPhonemes, 
    testPhonemesMatch,
    testAnyLine,
    testLastWord,
    testRhymes,
    testRhymesPattern,
    testRhymeIn,
    testStressLine,  
    testNSyllables,
    testMonad,
    testHaiku,
    testAba,
    testLex
    ])
  return ()
