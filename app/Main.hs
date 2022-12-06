module Main (main) where

import System.Environment

import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)
import qualified Day4 (part1, part2)
import qualified Day5 (part1, part2)

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
            _ -> "Not yet implemented"