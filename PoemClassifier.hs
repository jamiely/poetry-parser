module PoemClassifier (classify) where

import PoemParser

-- | Use to classify a poem. Given a poem, this will output information
-- about its type, rhyming scheme, and meter.
classify :: String -> String
classify _ = "Rhyming poem aa"

type Phoneme = String

data PoemPart = 
    Word String Int String [Phoneme] -- actual word, syllables, stress, phonemes
  | Break -- such as a linebreak

-- | Given a list of tokens, returns an analysis of the poem pieces
-- including breaks
analyze :: [Token] -> [PoemPart]
analyze _ = [Word "null" 1 "_" ["none"], Break]

