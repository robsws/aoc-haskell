module Grid (
    Grid (..),
    Coord (..),
    GridLookupError (..),
    get,
    getColumn,
    getRow,
    set,
    allCoords
) where

import Data.Maybe (fromJust, isJust)

data Grid a = Grid [[a]] deriving (Eq,Ord,Show,Read)
data Coord = Coord Int Int deriving (Eq,Ord,Show,Read)
data GridLookupError = GridLookupError String

checkCoord :: Coord -> Grid a -> Maybe GridLookupError
checkCoord _ (Grid []) = Just (GridLookupError "Cannot lookup from empty grid.")
checkCoord (Coord x y) (Grid rows)
    | x < 0 = Just (GridLookupError ("x value (" ++ (show x) ++ ") in lookup is below zero: " ++ (show x)))
    | y < 0 = Just (GridLookupError ("y value (" ++ (show y) ++ ") in lookup is below zero: " ++ (show y)))
    | x >= xSize = Just (GridLookupError ("x value (" ++ (show x) ++ ") in lookup is greater than length of row: " ++ show x))
    | y >= ySize = Just (GridLookupError ("y value (" ++ (show y) ++ ") in lookup is greater than length of column: " ++ show y))
    | otherwise = Nothing
    where xSize = length (rows!!0)
          ySize = length rows

get :: Coord -> Grid a -> Either GridLookupError a
get coord@(Coord x y) grid@(Grid rows)
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right ((rows!!y)!!x)
    where coordError = checkCoord coord grid

getRow :: Int -> Grid a -> Either GridLookupError [a]
getRow y grid@(Grid rows) 
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (rows!!y)
    where coordError = checkCoord (Coord 0 y) grid

getColumn :: Int -> Grid a -> Either GridLookupError [a]
getColumn x grid@(Grid rows)
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (map (!!x) rows)
    where coordError = checkCoord (Coord x 0) grid

set :: Coord -> a -> Grid a -> Either GridLookupError (Grid a)
set coord val grid
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (setImpl coord val grid)
    where coordError = checkCoord coord grid

setImpl :: Coord -> a -> Grid a -> Grid a
setImpl _ _ (Grid []) = error ("Went out of bounds of grid.")
setImpl (Coord x y) val (Grid (hRow:tRows))
    | y == 0 =
        let hRow' = setInRow x val hRow
            in Grid (hRow':tRows)
    | otherwise =
        let Grid tRows' = setImpl (Coord x (y-1)) val (Grid tRows)
            in Grid (hRow:tRows')

setInRow :: Int -> a -> [a] -> [a]
setInRow _ _ [] = error ("Went out of bounds of row.")
setInRow x val (hItem:tItems)
    | x == 0 = val:tItems
    | otherwise = 
        let tItems' = setInRow (x-1) val tItems
            in hItem:tItems'

allCoords :: Grid a -> [Coord]
allCoords (Grid []) = []
allCoords (Grid rows) = 
    [Coord x y | x <- [0..xSize-1], y <- [0..ySize-1]]
    where xSize = length (rows!!0)
          ySize = length rows