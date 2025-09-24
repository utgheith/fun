HS_FILES = ${wildcard *.hs}

all : fun

fun : Makefile ${HS_FILES}
	ghc -Wall ${HS_FILES}

%.run : fun %.fun
	./fun < $*.fun

clean:
	rm -f ${HS_PROGS} *.o *.hi fun
