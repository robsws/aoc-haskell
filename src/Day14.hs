module Day14 (part1, part2) where

import Control.Monad.State (State, runState, get, put)
import Coord (Coord(..), range)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe ( fromJust )

import Parsing (getAllIntsOnLine)
import Common (chunksOf, window)

parseInputs :: [String] -> Set Coord
parseInputs [] = Set.empty
parseInputs (l:ls) =
    let ints = getAllIntsOnLine l
        coords = [ Coord x y | [x,y] <- chunksOf 2 ints ]
        vectors = window 2 coords
        points = concat $ map (\w -> range (w!!0) (w!!1)) vectors
    in Set.union (Set.fromList points) (parseInputs ls)

simulateSand :: Coord -> Int -> Bool -> State (Set Coord) (Maybe Coord)
-- simulateSand grain maxY | trace ("simulateSand " ++ (show grain) ++ " " ++ (show maxY)) False = undefined
simulateSand grain@(Coord x y) maxY floorOn = do
    solidPoints <- get
    if not floorOn && y > maxY then
        return Nothing
    else if floorOn && y > maxY then
        return (Just grain)
    else if Set.notMember down solidPoints then
        simulateSand down maxY floorOn
    else if Set.notMember left solidPoints then
        simulateSand left maxY floorOn
    else if Set.notMember right solidPoints then
        simulateSand right maxY floorOn
    else
        return (Just grain)
    where
        down = Coord x (y+1)
        left = Coord (x-1) (y+1)
        right = Coord (x+1) (y+1)

dropSandUntilVoid :: Int -> Int -> State (Set Coord) Int
dropSandUntilVoid count maxY = do
    solidPoints <- get
    result <- simulateSand (Coord 500 0) maxY False
    case result of
        Nothing -> return count
        Just grain -> do
            put $ Set.insert grain solidPoints
            dropSandUntilVoid (count+1) maxY

dropSandUntilBlocked :: Int -> Int -> State (Set Coord) Int
dropSandUntilBlocked count maxY = do
    solidPoints <- get
    if Set.member start solidPoints then
        return count
    else do
        result <- simulateSand start maxY True
        grain <- return $ fromJust result
        put $ Set.insert grain solidPoints
        dropSandUntilBlocked (count+1) maxY
    where start = Coord 500 0

part1 :: [String] -> Either String String
part1 inputs = 
    let solidPoints = parseInputs inputs
        maxY = maximum (map (\(Coord _ y) -> y) (Set.toList solidPoints))
        (amount, _) = runState (dropSandUntilVoid 0 maxY) solidPoints
    in Right $ show amount

part2 :: [String] -> Either String String
part2 inputs = 
    let solidPoints = parseInputs inputs
        maxY = maximum (map (\(Coord _ y) -> y) (Set.toList solidPoints))
        (amount, _) = runState (dropSandUntilBlocked 0 maxY) solidPoints
    in Right $ show amount