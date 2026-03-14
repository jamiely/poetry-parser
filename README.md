# Poetry Parser

Jamie Ly (<jamiely>) and Nicholas McAvoy (<mcavoyna>)

A legacy Haskell (2011) poetry parser/classifier, modernized to compile and run on current toolchains (GHC 9.x, Docker, 2026-era systems).

## Quickstart (30 seconds)

```bash
docker build -t poetry-parser .
printf 'fox\nvision\npox\n' | docker run --rm -i poetry-parser
```

Expected final line:

```text
Type: Rhyming poem: aba
```

## What it does

Given a poem on `stdin`, the app tries to classify it as one of:
- Rhyming poem (`aba`)
- Rhyming poem (`aabba`)
- Haiku
- Limerick
- Iambic pentameter
- Shakespearean-sonnet style rhyme schemes

If no parser matches, it prints `unsupported form`.

## Modernization notes

This repository was updated to run on modern Haskell environments:

- **Prelude `Word` conflict fixed**
  - The project defines its own `Word` type. Modern Prelude also exports `Word`, causing ambiguity.
  - Fixed by hiding Prelude’s `Word` where needed.

- **Parser instances updated for modern GHC**
  - Older code defined only `Monad` for `PoemParser`.
  - Modern GHC requires `Functor` and `Applicative` superclasses too.
  - Added `Functor` and `Applicative` instances.

- **Dictionary handling made robust**
  - Original code used a hardcoded dictionary path.
  - Now dictionary path selection is:
    1. `CMUDICT_PATH` (if set)
    2. `extra/cmudict.0.7a`
    3. `dictionary-small.txt` fallback

- **Build setup modernized**
  - Removed obsolete platform-specific linker flags in `Makefile`.
  - Added Docker support for reproducible builds/runs.

## Dictionary data

Download a full CMU dictionary into `extra/cmudict.0.7a`:

```bash
make dictionary
```

You can still run without this file; the app will use `dictionary-small.txt`.

## Build (local)

Requirements:
- `ghc`
- `make`
- `HUnit` package (`libghc-hunit-dev` on Debian/Ubuntu)

Build:

```bash
make clean
make Main
```

## Run (local)

```bash
./Main < extra/fox.rhyming
./Main < extra/arb.hku
```

Use a custom dictionary:

```bash
CMUDICT_PATH=/path/to/cmudict ./Main < extra/fox.rhyming
```

## Docker (recommended)

Build image:

```bash
docker build -t poetry-parser .
```

Run with sample input file:

```bash
docker run --rm -i poetry-parser < extra/fox.rhyming
```

Run with inline sample input:

```bash
printf 'fox\nvision\npox\n' | docker run --rm -i poetry-parser
```

## Sample input/output

### Example 1: recognized rhyming pattern (`aba`)

Input:

```text
fox
vision
pox
```

Output:

```text
Using dictionary: dictionary-small.txt
Poem:
fox
vision
pox

Dictionary:

FOX  F AA1 K S
POX  P AA1 K S
VISION  V IH1 ZH AH0 N
Type: Rhyming poem: aba
```

### Example 2: unsupported form

Input (`extra/fox.rhyming`):

```text
something something fox
apple jacks
word here pox
```

Output:

```text
Using dictionary: dictionary-small.txt
Poem:
something something fox
apple jacks
word here pox

Dictionary:

FOX  F AA1 K S
POX  P AA1 K S
SOMETHING  S AH1 M TH IH0 NG
Type: unsupported form
```

## Running tests

In `ghci`:

```haskell
:l PoemClassifier
PoemClassifier.test

:l PoemAnalyzer
PoemAnalyzer.test
```

## Code layout

- `Main.hs`: reads poem input, finds dictionary, runs classifier
- `PoemClassifier.hs`: chooses poem type by applying parsers
- `CMUPronouncingDictionary.hs`: dictionary parsing and word metadata
- `PoemAnalyzer.hs`: converts strings into analyzed word data
- `PoemParser.hs`: parser combinators and poem-form parsers
