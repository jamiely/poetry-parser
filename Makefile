Main:
# -optl"-Wl,-read_only_relocs,suppress" is useful to prevent massive
#  warnings on OSX Lion. This has not been tested on other platforms.
# no_compact_unwind specifically prevents a linker error that occurs on
# Lion.
	cabal install split # not part of default install, provides Data.List.Split
	ghc --make -optl"-Wl,-read_only_relocs,suppress,-no_compact_unwind" Main

clean:
	rm Main

dictionary:
	curl -OL https://cmusphinx.svn.sourceforge.net/svnroot/cmusphinx/trunk/cmudict/cmudict.0.7a
	curl -OL https://cmusphinx.svn.sourceforge.net/svnroot/cmusphinx/trunk/cmudict/cmudict.0.7a.phones
	curl -OL https://cmusphinx.svn.sourceforge.net/svnroot/cmusphinx/trunk/cmudict/cmudict.0.7a.symbols
	mkdir dictionary
	mv cmudict.0.7a* dictionary/

