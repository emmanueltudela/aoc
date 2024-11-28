getFloor :: String -> Int
getFloor =
    foldl (\acc c -> if c == '(' then acc + 1 else acc - 1) 0

getFirstBasement :: String -> Int
getFirstBasement text =
    aux text 0 0
    where aux "" floor step =
              if floor < 0 then step else (-1)
          aux text@(x:xs) floor step
              | floor < 0 =
                  step
              | otherwise =
                  if x == '(' then
                      aux xs (floor + 1) (step + 1)
                  else
                      aux xs (floor - 1) (step + 1)

main = do
    content <- readFile "inputs/day1.txt"
    content <- return $ filter (\c -> c /= '\n') content

    -- Part One
    floorNumber <- return $ getFloor content
    print floorNumber

    -- Part Two
    firstBasement <- return $ getFirstBasement content
    print firstBasement
