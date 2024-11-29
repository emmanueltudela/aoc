solve _ =

main = do
    content <- readFile "inputs/day1.txt"

    -- Part One
    result <- return $ solve _
    print result
