GHC ?= ghc
GHC_FLAGS ?= -O2 -Wall -package HUnit

Main: Main.hs CMUPronouncingDictionary.hs PoemAnalyzer.hs PoemParser.hs PoemClassifier.hs
	$(GHC) --make $(GHC_FLAGS) Main.hs -o Main

run: Main
	./Main < extra/fox.rhyming

clean:
	rm -f Main *.hi *.o

# Downloads CMU dict into the legacy location expected by the app.
dictionary:
	curl -L https://raw.githubusercontent.com/cmusphinx/cmudict/master/cmudict.dict -o extra/cmudict.0.7a
