module Main (main) where

import System.Environment

import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)
import qualified Day4 (part1, part2)
import qualified Day5 (part1, part2)
import qualified Day6 (part1, part2)
import qualified Day7 (part1, part2)
import qualified Day8 (part1, part2)
import qualified Day9 (part1, part2)
import qualified Day10 (part1, part2)

main :: IO ()
main = do
    args <- getArgs
    input <- getContents

    let day = read (args !! 0) :: Int
        part = read (args !! 1) :: Int in

        putStrLn $ case (day, part) of
            (1,1) -> Day1.part1 $ lines input
            (1,2) -> Day1.part2 $ lines input
            (2,1) -> Day2.part1 $ lines input
            (2,2) -> Day2.part2 $ lines input
            (3,1) -> Day3.part1 $ lines input
            (3,2) -> Day3.part2 $ lines input
            (4,1) -> Day4.part1 $ lines input
            (4,2) -> Day4.part2 $ lines input
            (5,1) -> Day5.part1 $ lines input
            (5,2) -> Day5.part2 $ lines input
            (6,1) -> Day6.part1 $ input
            (6,2) -> Day6.part2 $ input
            (7,1) -> Day7.part1 $ lines input
            (7,2) -> Day7.part2 $ lines input
            (8,1) -> Day8.part1 $ lines input
            (8,2) -> Day8.part2 $ lines input
            (9,1) -> Day9.part1 $ lines input
            (9,2) -> Day9.part2 $ lines input
            (10,1) -> Day10.part1 $ lines input
            (10,2) -> Day10.part2 $ lines input
            _ -> "Not yet implemented"