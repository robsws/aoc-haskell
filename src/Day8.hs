module Day8 (part1, part2) where

import Grid

inputsToInts :: [String] -> [[Int]]
inputsToInts lLines = map (map (\c -> read [c])) lLines

countVisibleTrees :: Grid Int -> Int
countVisibleTrees (Grid []) = 0
countVisibleTrees grid =
    length $ filter (\c -> isVisible c grid) coords
    where coords = Grid.allCoords grid

isVisible :: Coord -> (Grid Int) -> Bool
isVisible coord@(Coord x y) grid =
    let treeH = case get coord grid of
            Left (GridLookupError err) -> error err
            Right val -> val
        row = case getRow y grid of
            Left (GridLookupError err) -> error err
            Right val -> val
        col = case getColumn x grid of
            Left (GridLookupError err) -> error err
            Right val -> val
        rowBefore = take x row
        rowAfter = drop (x+1) row
        colBefore = take y col
        colAfter = drop (y+1) col in
            length rowBefore == 0 ||
            length rowAfter == 0 ||
            length colBefore == 0 ||
            length colAfter == 0 ||
            foldr1 (&&) (map (<treeH) rowBefore) ||
            foldr1 (&&) (map (<treeH) rowAfter) ||
            foldr1 (&&) (map (<treeH) colBefore) ||
            foldr1 (&&) (map (<treeH) colAfter)

part1 :: [String] -> String
part1 inputs =
    let jungle = Grid.Grid (inputsToInts inputs) in
        show $ countVisibleTrees jungle

part2 :: [String] -> String
part2 inputs = ""