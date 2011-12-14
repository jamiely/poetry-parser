module PoemParser(Token (TokWord, TokNewline), PoemParser.lex, haiku,
 PoemParser, RhymeMap, doParse) where

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
lex = concatMap (fun1 . fun2) where
  fun1 = \l -> l ++ [TokNewline]
  fun2 = (foldr foldLine [])
  foldLine :: Word -> [Token] -> [Token]
  foldLine w toks = (TokWord w):toks

testLex :: Test
testLex = TestList [
  PoemParser.lex [[testWordFox, testWordPox], [testWordVision, testWordTransfusion]] ~?= 
    [TokWord testWordFox, TokWord testWordPox, TokNewline, TokWord testWordVision, TokWord testWordTransfusion, TokNewline] 
  ]

type RhymeStats = ([Phoneme], String)

type RhymeMap = Map String RhymeStats

newtype PoemParser a = P ([Token] -> [(a, [Token])])

doParse :: PoemParser a -> [Token] -> [(a, [Token])]
doParse (P p) pls = p pls


-- | If the first item in the input stream is the last word
-- on a line, parses a rhyme map from that word.
lastWord :: PoemParser RhymeMap
lastWord = P fun where
  fun ((TokWord w):TokNewline:vs) = [(map, vs)] where
    -- the rhyming scheme should be assigned using a previous map,
    -- using keys that don't exist
    map = Map.fromList [("a", (phonemes w, "a"))]
  fun _ = []

testLastWord :: Test
testLastWord = "Test lastWord" ~: TestList [
  doParse lastWord tokList ~?= [(map, [])]
  ] where
  tokList = [TokWord testWordFox, TokNewline]
  map = Map.fromList [("a", (["F", "AA1", "K", "S"], "a"))]
 
-- | Returns true if the last n phonemes in the passed lists match
phonemesMatch :: Int -> [Phoneme] -> [Phoneme] -> Bool
phonemesMatch n a b = part a == part b where
  part = (take n) . reverse

testPhonemesMatch :: Test
testPhonemesMatch = "Test phonemesMatch" ~: TestList [
  phonemesMatch 3 ["a", "b", "c"] ["x", "x", "a", "b", "c"] ~?= True
  ]

-- | Parses any word
anyWord :: PoemParser RhymeMap
anyWord = P fun where
  fun ((TokWord w):ts) = [(Map.empty, ts)]
  fun _                = []

testAnyWord :: Test
testAnyWord = "Test anyWord" ~: TestList [
  doParse anyWord tokList ~?= [(Map.empty, tail tokList)],
  doParse anyWord [] ~?= []
  ] where
  tokList = [TokWord testWordVision, TokWord testWordFox]

chooseP :: PoemParser a -> PoemParser a -> PoemParser a
p1 `chooseP` p2 = P (\cs -> let ls1 = doParse p1 cs in   --ls1 :: [(a,String)]
                            let ls2 = doParse p2 cs in   --ls2 :: [(a,String)]
                            ls1 ++ ls2)

-- | Use to determine if two words rhyme. Compares the last 3 phonemes
-- of each word to determine if they rhyme.
rhymes :: Word -> Word -> Bool
rhymes (Word _ _ _ ph1) (Word _ _ _ ph2) = phonemesMatch 3 ph1 ph2

testRhymes :: Test
testRhymes = "Test Rhymes" ~: TestList [
  "-zion" ~: rhymes testWordTransfusion testWordVision ~?= True,
  "-ox" ~: rhymes testWordFox testWordPox ~?= True,
  "doesn't rhyme" ~: rhymes testWordPox testWordVision ~?= False
  ] 

-- | Takes a RhymeMap and gives a parser which consumes input
-- that rhymes with something in that map.
rhymeIn :: RhymeMap -> PoemParser RhymeMap 
rhymeIn map = P fun where
  -- @todo This may need to be modified so that the map is agumented.
  fun ((TokWord word):ts) = if null matches then [] 
                            else [(map, ts)] where 
    matches = filter (filt word) $ Map.elems map 
  fun _                   = []

  filt word (phs, _) = rhymesWith word phs
  rhymesWith word = phonemesMatch 3 (phonemes word)

testRhymeIn :: Test
testRhymeIn = "Test rhymeIn" ~: TestList [
  doParse r [fox, vision] ~?= [(rhymeMap, [TokWord testWordVision])],
  doParse r [vision, fox] ~?= []
  ] where
  r = rhymeIn rhymeMap
  fox = TokWord testWordFox
  vision = TokWord testWordVision
  rhymeMap = Map.fromList [
    ("a", (["AA1", "K", "S"], "a"))
    ]

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
    testPhonemesMatch,
    testAnyWord,
    testLastWord,
    testRhymes,
    testRhymeIn,
    testNSyllables,
    testMonad,
    testHaiku,
    testLex
    ])
  return ()
