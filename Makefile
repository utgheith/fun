all : fun

fun : fun.hs
	ghc fun.hs

run : fun
	./fun

