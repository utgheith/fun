import FunSyntax (parse, Term)

eval :: Term -> (IO (), Maybe Integer)
eval = undefined

main = do
    prog <- getContents
    let (ast, rest) = parse prog
    if not (null rest) then
        error ("Unconsumed input: " ++ show rest)
    else do
        let (io, out) = eval ast
        io
        print out
