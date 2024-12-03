import Data.List.Split(splitOn)

diffsByTwo :: [Int] -> [Int]
diffsByTwo [] = []
diffsByTwo [ y ] = []
diffsByTwo (x : xs@(y : t)) =
    ((y - x):(diffsByTwo xs))

allSameSign :: [Int] -> Bool
allSameSign [] = True
allSameSign [ y ] = True
allSameSign (x : xs@(y : t))
    | x > 0 && y > 0 = allSameSign xs
    | x < 0 && y < 0 = allSameSign xs
    | otherwise      = False

inRange :: [Int] -> Bool
inRange [] = True
inRange (x:xs) =
    if (abs x) <= 3 then
        inRange xs
    else
        False

allSameSignArr :: [Int] -> [Bool]
allSameSignArr [] = []
allSameSignArr [ y ] = [True]
allSameSignArr (x : xs@(y : t))
    | x > 0 && y > 0 = True:(allSameSignArr xs)
    | x < 0 && y < 0 = True:(allSameSignArr xs)
    | otherwise      = False:(allSameSignArr xs)

inRangeArr :: [Int] -> [Bool]
inRangeArr [] = []
inRangeArr (x:xs) =
    if (abs x) <= 3 then
        True:(inRangeArr xs)
    else
        False:(inRangeArr xs)

count :: (Eq a) => a -> [a] -> Int
count occ l = foldl (\acc x -> if x == occ then acc + 1 else acc) 0 l

main = do
    content <- readFile "inputs/day2.txt"
    contentLines <- return $ lines content
    contentSplit <- return $ map (splitOn " ") contentLines
    contentNumbers <- return $ map (map (read :: String -> Int)) contentSplit
    contentDiffs <- return $ map diffsByTwo contentNumbers
    contentStable <- return $ map (\l -> allSameSign l && inRange l) contentDiffs
    result <- return $ count True contentStable
    print result

    contentSignsArr <- return $ map (allSameSignArr) contentDiffs
    contentRangeArr <- return $ map (inRangeArr) contentDiffs
    contentStableArr <- return $ zipWith (zipWith (\x y -> x && y)) contentSignsArr contentRangeArr
    result <- return $ length (filter (\l -> count False l <= 1) contentStableArr)
    print result
