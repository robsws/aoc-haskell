module Day12 (part1, part2) where

import Grid (Grid(..), GridLoc(..), Coord(..), get, manhattan, find, findAll, set, GridLookupError(..))
import Graph (GraphNode(..), astar)

import Data.List ( elemIndex ) 
import Data.Maybe ( fromJust )

newtype HeightGrid = HeightGrid { getGrid :: Grid Int } deriving (Eq, Show)
data HeightGridLoc = HeightGridLoc Coord HeightGrid deriving (Eq)

instance Show HeightGridLoc where
    show (HeightGridLoc coord _) = show coord 

fromGridLoc :: GridLoc Int -> HeightGridLoc
fromGridLoc (GridLoc c g) = HeightGridLoc c (HeightGrid g)

instance GraphNode HeightGridLoc where
    adjacent (HeightGridLoc coord hGrid) =
        let neighbours = filter (\((GridLoc c _), _) -> reachableFrom hGrid coord c) allAdj
        in map (\(loc, cost) -> (fromGridLoc loc, cost)) neighbours
        where grid = getGrid hGrid
              allAdj = adjacent (GridLoc coord grid)

reachableFrom :: HeightGrid -> Coord -> Coord -> Bool
reachableFrom hGrid start end =
    either (\_->False) (\b->b) (reachableImpl)
    where grid = getGrid hGrid
          reachableImpl = do
              myHeight <- get start grid
              theirHeight <- get end grid
              return $ theirHeight <= myHeight + 1

parseInputs :: [String] -> Either GridLookupError (HeightGrid, Coord, Coord)
parseInputs inputs =
    let possHeights = ['S'] ++ ['a'..'z'] ++ ['E']
        cToI = (\c -> fromJust $ elemIndex c possHeights)
        grid = Grid (map (map cToI) inputs)
    in do
        start <- find 0 grid
        end <- find (length possHeights - 1) grid
        grid' <- set start 1 grid
        grid'' <- set end (length possHeights - 2) grid'
        return (HeightGrid grid'', start, end)

getBestRouteFromLocs :: HeightGrid -> [HeightGridLoc] -> HeightGridLoc -> Int
getBestRouteFromLocs _ [] _ = maxBound
getBestRouteFromLocs hGrid (startLoc:startLocs) endLoc@(HeightGridLoc end _) =
    case result of
        Nothing -> recursiveResult
        Just cost -> min cost recursiveResult
    where result = astar startLoc endLoc (\(HeightGridLoc c _) -> manhattan end c)
          recursiveResult = getBestRouteFromLocs hGrid startLocs endLoc

part1 :: [String] -> Either String String
part1 inputs = do
    case parseInputs inputs of
        Left (GridLookupError err) ->
            return err
        Right (hGrid, start, end) ->
            let startLoc = HeightGridLoc start hGrid
                endLoc = HeightGridLoc end hGrid
                cost = astar startLoc endLoc (\(HeightGridLoc c _) -> manhattan end c)
            in return $ show cost

part2 :: [String] -> Either String String
part2 inputs = do
    case parseInputs inputs of
        Left (GridLookupError err) ->
            return err
        Right (hGrid, _, end) ->
            let grid = getGrid hGrid
                startLocs = map (\coord -> HeightGridLoc coord hGrid) (findAll 1 grid)
                endLoc = HeightGridLoc end hGrid
                cost = getBestRouteFromLocs hGrid startLocs endLoc
            in return $ show cost