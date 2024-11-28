import Utils.Box
import Data.List.Split(splitOn)

getWrappingPaperSurface :: [Box] -> Int
getWrappingPaperSurface boxes =
    let boxesAreas = map (getBoxArea) boxes
        boxesSmallestSideArea = map (getBoxSmallestSideArea) boxes
    in  (sum boxesAreas) + (sum boxesSmallestSideArea)

getFeetOfRibbon :: [Box] -> Int
getFeetOfRibbon boxes =
    let boxesSmallestPerimeters = map (getBoxSmallestSidePerimeter) boxes
        boxesVolumes = map (getBoxVolume) boxes
    in  (sum boxesSmallestPerimeters) + (sum boxesVolumes)

main = do
    content <- readFile "inputs/day2.txt"
    boxesSplit <- return $ filter (\s -> s /= "") (splitOn "\n" content)
    boxesSizesSplit <- return $ map (splitOn "x") boxesSplit
    boxesSizes <- return $ map (map (read :: String -> Int)) boxesSizesSplit
    boxes <- return $ map (listToBox) boxesSizes

    -- Part One
    wrappingPaperSurface <- return $ getWrappingPaperSurface boxes
    print wrappingPaperSurface

    -- Part Two
    feetOfRibbons <- return $ getFeetOfRibbon boxes
    print feetOfRibbons
