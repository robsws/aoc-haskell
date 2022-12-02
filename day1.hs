module Day1 (part1, part2) where

import Data.List

splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = [[]]
splitByEmpty ([]:strings) = [] : splitByEmpty strings
splitByEmpty (string:strings) =
    let group:groups = splitByEmpty strings in
        (string:group) : groups

elfCaloriesRanked :: [String] -> [Int]
elfCaloriesRanked ls =
    reverse . sort . -- Sort largest to smallest
    map sum . -- Add up all calories for each elf
    map (map read) -- Convert all values to ints
        $ splitByEmpty ls -- Subdivide list using empty lines as delimiters

part1 :: [String] -> String
part1 inputs = show . head $ elfCaloriesRanked inputs

part2 :: [String] -> String
part2 inputs = show . sum . take 3 $ elfCaloriesRanked inputs