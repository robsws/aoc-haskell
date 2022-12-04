module Day1 (part1, part2) where

import Common (splitBy)
import Data.List (sort)

elfCaloriesRanked :: [String] -> [Int]
elfCaloriesRanked ls =
    reverse . sort . -- Sort largest to smallest
    map sum . -- Add up all calories for each elf
    map (map read) -- Convert all values to ints
        $ splitBy "" ls -- Subdivide list using empty lines as delimiters

part1 :: [String] -> String
part1 inputs = show . head $ elfCaloriesRanked inputs 

part2 :: [String] -> String
part2 inputs = show . sum . take 3 $ elfCaloriesRanked inputs