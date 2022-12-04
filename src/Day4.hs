module Day4 (part1, part2) where

import Common

parseLine :: String -> (Int, Int, Int, Int)
parseLine l =
    let ints = map read . -- convert to ints
               concat . -- flatten list -> [".",".",".","."]
               map (splitBy '-') -- split parts by '-' -> [[".","."],[".","."]]
                   $ splitBy ',' l  in -- split whole string by ',' -> [".-.",".-."]
        (ints !! 0, ints !! 1, ints !! 2, ints !! 3)

fullyContains :: (Int,Int) -> (Int,Int) -> Bool
range1 `fullyContains` range2 =
    (fst range1 <= fst range2 && snd range1 >= snd range2)

overlaps :: (Int, Int) -> (Int, Int) -> Bool
range1 `overlaps` range2 =
    (fst range1 <= fst range2 && snd range1 >= fst range2) ||
    (fst range1 <= snd range2 && snd range1 >= snd range2) ||
    range1 `fullyContains` range2 ||
    range2 `fullyContains` range1

testLineForContainership :: String -> Bool
testLineForContainership l =
    let (r1l, r1u, r2l, r2u) = parseLine l in
        (r1l, r1u) `fullyContains` (r2l, r2u) ||
        (r2l, r2u) `fullyContains` (r1l, r1u)

testLineForOverlap :: String -> Bool
testLineForOverlap l =
    let (r1l, r1u, r2l, r2u) = parseLine l in
        (r1l, r1u) `overlaps` (r2l, r2u)

part1 :: [String] -> String
part1 inputs =
    show . length $ filter testLineForContainership inputs

part2 :: [String] -> String
part2 inputs =
    show . length $ filter testLineForOverlap inputs