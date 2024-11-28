module (
, Direction
, Map
) where

data Direction = North | South | East | West | Here deriving (Show, Eq)
data Map = Map { d :: Direction, x :: Int, y :: Int } deriving(Show, Eq)
