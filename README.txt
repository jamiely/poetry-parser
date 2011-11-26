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

# Running Tests

In ghci:

    :l PoemClassifier
    PoemClassifier.test

    :l PoemAnalyzer
    PoemAnalyzer.test

