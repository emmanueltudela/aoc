module Box.Box
( Box
, listToBox
, getBoxSidesAreas
, getBoxSmallestSideArea
, getBoxArea
, getBoxSidesPerimeters
, getBoxSmallestSidePerimeter
, getBoxVolume
) where

data Box = Box { l :: Int, w :: Int, h :: Int } deriving(Show)

listToBox :: [Int] -> Box
listToBox [l, w, h] = Box l w h

getBoxSidesAreas :: Box -> (Int, Int, Int)
getBoxSidesAreas (Box l w h) = (l * w, w * h, h * l)

getBoxSmallestSideArea :: Box -> Int
getBoxSmallestSideArea b@(Box l w h) =
    let (s1, s2, s3) = getBoxSidesAreas b
    in  min (min s1 s2) s3

getBoxArea :: Box -> Int
getBoxArea b =
    2 * s1 + 2 * s2 + 2 * s3
    where (s1, s2, s3) = getBoxSidesAreas b

getBoxSidesPerimeters :: Box -> (Int, Int, Int)
getBoxSidesPerimeters (Box l w h) = (2 * l + 2 * w, 2 * w + 2 * h, 2 * h + 2 * l)

getBoxSmallestSidePerimeter :: Box -> Int
getBoxSmallestSidePerimeter b@(Box l w h) =
    let (p1, p2, p3) = getBoxSidesPerimeters b
    in  min (min p1 p2) p3

getBoxVolume :: Box -> Int
getBoxVolume (Box l w h) = l * w * h
