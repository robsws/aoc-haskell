module Day9 (part1, part2) where

import Grid (Coord (Coord))

import qualified Data.Set as Set (Set, insert, singleton, toList)
import Debug.Trace (trace)

type Visited = Set.Set Coord
data Movement = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Show)
data RopeState = RopeState Coord Coord Visited deriving (Show)

parseInputs :: [String] -> [Movement]
parseInputs [] = []
parseInputs (hLine:tLines)
    | dir == "L" = replicate amount MoveLeft ++ parseInputs tLines
    | dir == "R" = replicate amount MoveRight ++ parseInputs tLines
    | dir == "U" = replicate amount MoveUp ++ parseInputs tLines
    | dir == "D" = replicate amount MoveDown ++ parseInputs tLines
    | otherwise = error ("Invalid direction: " ++ dir)
    where lineParts = words hLine
          dir = lineParts!!0
          amount = read (lineParts!!1)

doMoves :: [Movement] -> RopeState -> RopeState
doMoves [] state = state
doMoves (hMove:tMoves) state =
    let state' = moveHead hMove state
        in doMoves tMoves state'

moveHead :: Movement -> RopeState -> RopeState
moveHead MoveLeft (RopeState (Coord hx hy) ropeTail visited) =
    moveTail (RopeState (Coord (hx-1) hy) ropeTail visited)
moveHead MoveRight (RopeState (Coord hx hy) ropeTail visited) =
    moveTail (RopeState (Coord (hx+1) hy) ropeTail visited)
moveHead MoveUp (RopeState (Coord hx hy) ropeTail visited) =
    moveTail (RopeState (Coord hx (hy-1)) ropeTail visited)
moveHead MoveDown (RopeState (Coord hx hy) ropeTail visited) =
    moveTail (RopeState (Coord hx (hy+1)) ropeTail visited)

moveTail :: RopeState -> RopeState
moveTail state@(RopeState ropeHead@(Coord hx hy) (Coord tx ty) visited)
    -- Tail is too far directly up or down of head
    | distance > 1 && xVec == 0 =
        -- Move tail towards head up or down
        let ropeTail' = Coord tx (ty+yDir)
            in (RopeState ropeHead ropeTail' (Set.insert ropeTail' visited))
    -- Tail is too far directly left or right of head
    | distance > 1 && yVec == 0 =
        -- Move tail towards head left or right
        let ropeTail' = Coord (tx+xDir) ty
            in (RopeState ropeHead ropeTail' (Set.insert ropeTail' visited))
    -- Tail is too far directly up or down of head and also one to left or right
    | distance > 2 && yMag > xMag =
        -- Move tail towards head up or down and back in line horizontally with head
        let ropeTail' = Coord hx (ty+yDir)
            in (RopeState ropeHead ropeTail' (Set.insert ropeTail' visited))
    -- Tail is too far directly left or right of head and also one up or down
    | distance > 2 && xMag > yMag =
        -- Move tail towards head left or right and back in line vertically with head
        let ropeTail' = Coord (tx+xDir) hy
            in (RopeState ropeHead ropeTail' (Set.insert ropeTail' visited))
    -- Tail is neighbouring head, so don't move it
    | otherwise = state
    where xVec = (hx-tx)
          xMag = abs xVec
          xDir = xVec `div` xMag
          yVec = (hy-ty)
          yMag = abs yVec
          yDir = yVec `div` yMag
          distance = (abs xVec) + (abs yVec)

part1 :: [String] -> String
part1 inputs =
    let initialState = RopeState (Coord 0 0) (Coord 0 0) (Set.singleton (Coord 0 0))
        moves = parseInputs inputs
        RopeState _ _ visited = doMoves moves initialState
            in show . length . Set.toList $ visited

part2 :: [String] -> String
part2 inputs = ""