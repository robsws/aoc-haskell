module Day9 (part1, part2) where

import Grid (Coord (Coord))

import qualified Data.Set as Set (Set, insert, singleton, toList)
import Debug.Trace (trace)

type Visited = Set.Set Coord
data Movement = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Show)
data RopeState = RopeState [Coord] Visited deriving (Show)

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
moveHead _ (RopeState [] _) = error ("Rope is empty, this base case should never be reached.")
moveHead MoveLeft (RopeState ((Coord hx hy):tRope) visited) =
    moveTail (RopeState ((Coord (hx-1) hy):tRope) visited)
moveHead MoveRight (RopeState ((Coord hx hy):tRope) visited) =
    moveTail (RopeState ((Coord (hx+1) hy):tRope) visited)
moveHead MoveUp (RopeState ((Coord hx hy):tRope) visited) =
    moveTail (RopeState ((Coord hx (hy-1)):tRope) visited)
moveHead MoveDown (RopeState ((Coord hx hy):tRope) visited) =
    moveTail (RopeState ((Coord hx (hy+1)):tRope) visited)

moveTail :: RopeState -> RopeState
moveTail (RopeState [] _) = error ("Rope is empty, this base case should never be reached.")
moveTail (RopeState (knot:[]) visited) =
    -- If this is the last knot in the rope (the tail),
    -- update the visited set with the current coordinates
    RopeState [knot] (Set.insert knot visited)
moveTail state@(RopeState (parentKnot@(Coord px py):(Coord kx ky):rope) visited)
    -- Knot is too far directly up or down of parent
    | distance > 1 && xVec == 0 =
        -- Move knot towards parent up or down
        let knot' = Coord kx (ky+yDir)
            (RopeState rope' visited') = moveTail (RopeState (knot':rope) visited)
                in (RopeState (parentKnot:rope') visited')
    -- Knot is too far directly left or right of head
    | distance > 1 && yVec == 0 =
        -- Move tail towards head left or right
        let knot' = Coord (kx+xDir) ky
            (RopeState rope' visited') = moveTail (RopeState (knot':rope) visited)
                in (RopeState (parentKnot:rope') visited')
    -- Knot is too far directly up or down of head and also one to left or right
    | distance > 2 && yMag > xMag =
        -- Move tail towards head up or down and back in line horizontally with head
        let knot' = Coord px (ky+yDir)
            (RopeState rope' visited') = moveTail (RopeState (knot':rope) visited)
                in (RopeState (parentKnot:rope') visited')
    -- Knot is too far directly left or right of head and also one up or down
    | distance > 2 && xMag > yMag =
        -- Move tail towards head left or right and back in line vertically with head
        let knot' = Coord (kx+xDir) py
            (RopeState rope' visited') = moveTail (RopeState (knot':rope) visited)
                in (RopeState (parentKnot:rope') visited')
    -- Knot is diagonally one too far away from parent
    | distance > 3 =
        -- Move knot diagonally towards parent
        let knot' = Coord (kx+xDir) (ky+yDir)
            (RopeState rope' visited') = moveTail (RopeState (knot':rope) visited)
                in (RopeState (parentKnot:rope') visited')
    -- Tail is neighbouring head, so don't move it
    | otherwise = state
    where xVec = (px-kx)
          xMag = abs xVec
          xDir = xVec `div` xMag
          yVec = (py-ky)
          yMag = abs yVec
          yDir = yVec `div` yMag
          distance = (abs xVec) + (abs yVec)

countRopeTailCoordsVisited :: [String] -> Int -> Int
countRopeTailCoordsVisited moves ropeLength = 
    let initialState = RopeState (replicate ropeLength (Coord 0 0)) (Set.singleton (Coord 0 0))
        moves' = parseInputs moves
        RopeState _ visited = doMoves moves' initialState
            in length . Set.toList $ visited

part1 :: [String] -> String
part1 inputs = show $ countRopeTailCoordsVisited inputs 2

part2 :: [String] -> String
part2 inputs = show $ countRopeTailCoordsVisited inputs 10