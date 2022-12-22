module Coord (
    Coord (..),
    range
) where

data Coord = Coord {getX :: Int, getY :: Int} deriving (Eq,Ord,Show,Read)

range :: Coord -> Coord -> [Coord]
range (Coord x1 y1) (Coord x2 y2) = 
    let loX = min x1 x2
        hiX = max x1 x2
        loY = min y1 y2
        hiY = max y1 y2
        xRange = [loX .. hiX]
        yRange = [loY .. hiY]
    in [Coord x y | x <- xRange, y <- yRange]