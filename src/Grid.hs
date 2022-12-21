module Grid (
    Grid (..),
    GridLoc (..),
    Coord (..),
    GridLookupError (..),
    get,
    getColumn,
    getRow,
    set,
    allCoords,
    find,
    findAll,
    manhattan,
    adjacent
) where

import Data.Maybe (fromJust, isJust, isNothing)

import Graph
import Coord

data Grid a = Grid [[a]] deriving (Eq,Ord,Show,Read)
data GridLookupError = GridLookupError String

data GridLoc a = GridLoc Coord (Grid a)

instance Show (GridLoc a) where
    show (GridLoc c _) = show c

instance GraphNode (GridLoc a) where
    adjacent (GridLoc (Coord x y) grid) =
        let n = Coord x (y-1)
            s = Coord x (y+1)
            w = Coord (x-1) y
            e = Coord (x+1) y
            valid = filter (isNothing . checkCoord grid) [n,s,w,e]
        in map (\c -> (GridLoc c grid,1)) valid

checkCoord :: Grid a -> Coord -> Maybe GridLookupError
checkCoord (Grid []) _ = Just (GridLookupError "Cannot lookup from empty grid.")
checkCoord (Grid rows) (Coord x y)
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
    where coordError = checkCoord grid coord

getRow :: Int -> Grid a -> Either GridLookupError [a]
getRow y grid@(Grid rows) 
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (rows!!y)
    where coordError = checkCoord grid (Coord 0 y)

getColumn :: Int -> Grid a -> Either GridLookupError [a]
getColumn x grid@(Grid rows)
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (map (!!x) rows)
    where coordError = checkCoord grid (Coord x 0)

set :: Coord -> a -> Grid a -> Either GridLookupError (Grid a)
set coord val grid
    | isJust coordError = Left (fromJust coordError)
    | otherwise = Right (setImpl coord val grid)
    where coordError = checkCoord grid coord

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

find :: (Eq a) => a -> Grid a -> Either GridLookupError Coord
find item grid = 
    let allFound = findAll item grid
    in if allFound == [] then 
        Left $ GridLookupError("Item not found in grid") 
    else 
        Right $ head allFound

findAll :: (Eq a) => a -> Grid a -> [Coord]
findAll item grid = 
    filter (\c -> (isItem (get c grid))) coords
    where coords = allCoords grid
          isItem (Left (GridLookupError err)) = error (err)
          isItem (Right item') = item == item'

manhattan :: Coord -> Coord -> Int
manhattan (Coord sx sy) (Coord ex ey) =
    (abs $ ex - sx) + (abs $ ey - sy)