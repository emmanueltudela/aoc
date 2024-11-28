getCoords :: (Int, Int) -> Char -> (Int, Int)
getCoords (x, y) c
    | c == '<' = (x - 1, y)
    | c == '^' = (x, y + 1)
    | c == '>' = (x + 1, y)
    | c == 'v' = (x, y - 1)
    | otherwise = (0, 0)

getDistributedHouses :: String -> Int
getDistributedHouses content =
    aux content 1 [(0, 0)] (0, 0)
    where aux [] distributed _ _ = distributed
          aux (c:xs) distributed crossed (x, y) =
              let newCoords = getCoords (x, y) c
              in  if newCoords `elem` crossed then
                      aux xs distributed crossed newCoords
                  else
                      aux xs (distributed + 1) (newCoords:crossed) newCoords

getDistributedHousesTwo :: String -> Int
getDistributedHousesTwo content =
    aux content 1 [(0, 0)] (0, 0) (0, 0) 0
    where aux [] distributed _ _ _ _ = distributed
          aux (c:xs) distributed crossed (x, y) (x', y') i
              | i `mod` 2 == 0 =
                  let newCoords = getCoords (x, y) c
                  in  if newCoords `elem` crossed then
                          aux xs distributed crossed newCoords (x', y') (i + 1)
                      else
                          aux xs (distributed + 1) (newCoords:crossed) newCoords (x', y') (i + 1)
              | otherwise      =
                  let newCoords = getCoords (x', y') c
                  in  if newCoords `elem` crossed then
                          aux xs distributed crossed (x, y) newCoords (i + 1)
                      else
                          aux xs (distributed + 1) (newCoords:crossed) (x, y) newCoords (i + 1)

main = do
    content <- readFile "inputs/day3.txt"
    content <- return $ filter (\c -> c /= '\n') content
    result <- return $ getDistributedHouses content
    print result

    result2 <- return $ getDistributedHousesTwo content
    print result2
