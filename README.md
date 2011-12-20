# Poetry Parser

Jamie Ly <jamiely> and Nicholas McAvoy <mcavoyna>

# Introduction

From the advent of spoken language, poetry has been a protocol for sharing human experience. We would like to write a poetry parser and classifier.

The parser will determine if a given string qualifies as any of the following English poetic forms.

Goal forms include (tentative):
- formless rhyming poem (each line ends in a word that rhymes with the last)
- haiku
- limerick
- Shakespearean sonnet

# Downloading libraries

Run the following command:

    make dictionary

# Building

    make clean
    make

# Running main/Usage

    ./Main < extra/fox.rhyming
    ./Main < extra/arb.hku

# Running Tests

In ghci:

    :l PoemClassifier
    PoemClassifier.test

    :l PoemAnalyzer
    PoemAnalyzer.test

# Flow of Program

  1. Read poem from STDIN
  2. Separate poem into words, normalize for dictionary lookup
  3. Build dictionary from `cmudict.0.7a` file
  4. Submit original poem and dictionary to classifier
  5. Process the poem into `Word` types, containing extended information
  6. Tokenize the `Word` lists
  7. Parse the tokens using the extended `Word` information
  8. Apply one of the parsers (rhymingPoem, haiku, limerick, sonnet) to classify 
     the poem type
  9. Retrieve a RhymeMap to characterize the rhyming scheme of the poem

# Main Components Description

  1. Read `Main.hs`. It reads in a dictionary file from a hardcoded
     location, and a poem from STDIN
  2. Next, `PoemClassifier.hs` determines the type of poem. It calls other
     modules.
  3. `CMUPronouncingDictionary` provides functions for manipulating the
     dictionary and turning it into a datastructure to search.
  4. `PoemAnalyzer` uses the `CMUPronouncingDictionary` to turn strings
     into Word types.
  5. `PoemParser.hs` contains the parsing Monad which determines a poem
     by parsing lists of Words.

