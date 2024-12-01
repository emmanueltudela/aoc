import Data.List.Split(splitOn)

solve _ = 1

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    (quicksort lesser) ++ [x] ++ (quicksort greater)
    where lesser = [y | y <- xs, y <= x]
          greater = [y | y <- xs, y > x]

count :: (Eq a) => a -> [a] -> Int
count occ l = foldl (\acc x -> if x == occ then acc + 1 else acc) 0 l

main = do
    content <- readFile "inputs/day1.txt"
    contentLines <- return $ lines content
    contentLinesSplit <- return $ map (splitOn "   ") contentLines
    contentLinesNumbers <- return $ map (map (read :: String -> Int)) contentLinesSplit
    (left, right) <- return $ (map (!!0) contentLinesNumbers, map (!!1) contentLinesNumbers)

    -- Part One
    (leftSort, rightSort) <- return $ (quicksort left, quicksort right)
    contentDiffs <- return $ zipWith (\x y -> (max x y) - (min x y)) leftSort rightSort
    result <- return $ sum contentDiffs
    print result

    -- Part Two
    occList <- return right
    contentValues <- return $ map (\x -> x * (count x occList)) left
    result <- return $ sum contentValues
    print result
