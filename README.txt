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

## Rhyming Poem

In a rhyming poem, each line ends in a word that rhymes with the last word of the previous line.

Sometimes rhyming schemes alternate like _aabbcc_.

> The quick brown fox
> caught the pox
> and gave it to the farmer
> who took it to barter
> but gave it for free
> to poor, dead Bree
- Jamie Ly

## Haiku

A haiku need not rhyme, but has a 3-line syllabic form. The first line has 5 syllables, followed by a 7-syllable line, completed by a 5 syllable linel.

> An old silent pond...
> A frog jumps into the pond,
> splash! Silence again.
- Basho

## Limerick

A limerick is characterized by the rhyming scheme: _aaba_.

> There was an Old Man with a beard,
> Who said, 'It is just as I feared!'
> Two Owls and a Hen,
> Four Larks and a Wren,
> Have all built their nests in my beard!
- Edward Lear

## Shakespearean Sonnet

A Shakespearean Sonnet uses the following rhyming scheme:
a-b-a-b, c-d-c-d, e-f-e-f, g-g. 

Additionally, each line has 10 syllables and is written in iambic pentameter. Thus, each group of two syllables in a line is unstressed, then stressed

> From fairest creatures we desire increase,
> That thereby beauty's rose might never die,
> But as the riper should by time decease,
> His tender heir might bear his memory:
> But thou contracted to thine own bright eyes,
> Feed'st thy light's flame with self-substantial fuel,
> Making a famine where abundance lies,
> Thy self thy foe, to thy sweet self too cruel:
> Thou that art now the world's fresh ornament,
> And only herald to the gaudy spring,
> Within thine own bud buriest thy content,
> And tender churl mak'st waste in niggarding:
> Pity the world, or else this glutton be,
> To eat the world's due, by the grave and thee.
- Shakespeare

It may also be feasible to generate poetry using the support functions we create.

For pronunciation information we will make use of the CMU Pronouncing Dictionary[1].

# Use cases

This tool could be used in a social media service similar to Twitter. Rather than being subject to a length restriction, users could only post if their content qualified as one of the above forms.

"Rick" has developed strong emotions toward his alarm clock. He would like to express them to his friends. He goes to the site and begins to enter a poem. The button to "post" doesn't activate until his text passes this parser.

"Cassandra" has a few choice words for the Republican presidential field. As a matter of fact, her choice words are in iambic pentameter. By making use of the above service backed with this tool, she is able to give her words a place above the lazy tweets and status updates.

"Nick" wants to know if his poetry parser is working. He types in one of Shakespeare's sonnets. To his delight, the text passes.

[1] https://cmusphinx.svn.sourceforge.net/svnroot/cmusphinx/trunk/cmudict/

# Break Down

There are several components that will be required:

- A lexer that creates tokens from a body of English text
- Parser builders based on word sounds and syllables
- 3 parsers based on parser builders above 

Some additional components which will be implemented if time permits:

- An arbitrary generator for the Poem type
- Support functions such as `rhymesWith` and `hasNSyllables` which will return word options
- More complex parser building functions based on expected parts of speech such as `article` `adjective` `noun` `verb`, which might parse "the yellow bird flies"
  - This could be used to detect and create more strict forms of poetry

# Effort Budget

## Core tasks

- Poetry should be lexed into tokens (1 hour)
- Users should be able to submit a rhyming poem which is recognized as a rhyming poem (15h)
- Users should be able to submit a haiku which is recognized
  as a haiku (5h)
- Users should be able to submit a limerick which is
  recognized as a limerick (2h)
- Users should be able to submit a Shakespearean sonnet which
  is recognized as a Shakespearean sonnet (15h)

## Optional tasks

- Users should be able to generate poetry that conforms to the above forms (they may not make sense). (5h)
- Users should be able to generate poetry that conforms to some of the forms above that make more senses by creating complex generators that generate more structured language (15h)

