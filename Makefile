HS_FILES = ${wildcard *.hs}
CONFIG_FILES = Makefile fun.cabal

all :
	@stack build

%.run : all %.fun
	@stack run < $*.fun

test :
	@stack test

clean:
	stack clean
