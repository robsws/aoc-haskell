module Graph (
    GraphNode(..),
    astar
) where

import Data.Maybe ( isJust )
import Data.List ( sortBy )
import Debug.Trace ()

class GraphNode n where
    adjacent :: n -> [(n, Int)]

type NodePath n = (n, Int)

upsert :: (Eq k) => [(k, v)] -> k -> v -> [(k, v)]
upsert assocList key value
    | isJust result = map (\(key', value') -> if key' == key then (key', value) else (key', value')) assocList
    | otherwise = (key,value):assocList
    where result = lookup key assocList

astar :: (Eq n, GraphNode n, Show n) => n -> n -> (n -> Int) -> Maybe Int
astar start end heuristic = fmap snd (astarImpl end heuristic [(start, 0)] [])

astarImpl :: (Eq n, GraphNode n, Show n) => n -> (n -> Int) -> [NodePath n] -> [NodePath n] -> Maybe (NodePath n)
astarImpl _ _ [] _ = Nothing
astarImpl end heuristic (nodePath@(node, cost):openSet) closedSet
    | node == end = Just nodePath
    | otherwise = astarImpl end heuristic openSet' closedSet'
    where
        closedSet' = upsert closedSet node cost
        moves = adjacent node
        moves' = filter (\(n,c) -> not $ isWorseMove closedSet' n (cost+c)) moves
        newPaths = [(n, cost+c) | (n,c) <- moves']
        newPaths' = filter (\np -> not $ np `elem` openSet) newPaths
        openSet' = sortBy (compareNodePaths heuristic) (newPaths' ++ openSet)

compareNodePaths :: (n -> Int) -> NodePath n -> NodePath n -> Ordering
compareNodePaths heuristic np1 np2 =
    compare (costFunc np1) (costFunc np2)
    where costFunc (node,cost) = heuristic node + cost

isWorseMove :: (Eq n) => [NodePath n] -> n -> Int -> Bool
isWorseMove closedSet node totalCost =
    case lookup node closedSet of
        Nothing -> False
        Just otherCost -> totalCost >= otherCost