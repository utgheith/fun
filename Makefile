HS_FILES = ${wildcard *.hs}
CONFIG_FILES = Makefile fun.cabal

all :
	@cabal build

%.run : all %.fun
	@cabal run < $*.fun

test :
	@cabal test

clean:
	cabal clean
