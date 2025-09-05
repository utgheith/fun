HS_FILES = ${wildcard *.hs}
HS_PROGS = ${HS_FILES:.hs=}

all : ${HS_PROGS}

${HS_PROGS}: % : %.hs
	ghc $*.hs

clean:
	rm -f ${HS_PROGS} *.o *.hi
