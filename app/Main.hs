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
import qualified Day11 (part1, part2)
import qualified Day12 (part1, part2)
import qualified Day13 (part1, part2)
import qualified Day14 (part1, part2)
import qualified Day15 (part1, part2)

main :: IO ()
main = do
    result <- doAOC
    case result of
        Left err -> putStrLn ("Error occured: " ++ err)
        Right value -> putStrLn value

doAOC :: IO (Either String String)
doAOC = do
    args <- getArgs
    input <- getContents
    day <- return (read (args !! 0) :: Int)
    part <- return (read (args !! 1) :: Int)
    case (day, part) of
        (1,1) -> return (return $ Day1.part1 $ lines input)
        (1,2) -> return (return $ Day1.part2 $ lines input)
        (2,1) -> return (return $ Day2.part1 $ lines input)
        (2,2) -> return (return $ Day2.part2 $ lines input)
        (3,1) -> return (return $ Day3.part1 $ lines input)
        (3,2) -> return (return $ Day3.part2 $ lines input)
        (4,1) -> return (return $ Day4.part1 $ lines input)
        (4,2) -> return (return $ Day4.part2 $ lines input)
        (5,1) -> return (return $ Day5.part1 $ lines input)
        (5,2) -> return (return $ Day5.part2 $ lines input)
        (6,1) -> return (return $ Day6.part1 $ input)
        (6,2) -> return (return $ Day6.part2 $ input)
        (7,1) -> return (return $ Day7.part1 $ lines input)
        (7,2) -> return (return $ Day7.part2 $ lines input)
        (8,1) -> return (return $ Day8.part1 $ lines input)
        (8,2) -> return (return $ Day8.part2 $ lines input)
        (9,1) -> return (return $ Day9.part1 $ lines input)
        (9,2) -> return (return $ Day9.part2 $ lines input)
        (10,1) -> return (return $ Day10.part1 $ lines input)
        (10,2) -> return (return $ Day10.part2 $ lines input)
        (11,1) -> return (return $ Day11.part1 $ lines input)
        (11,2) -> return (return $ Day11.part2 $ lines input)
        (12,1) -> return $ Day12.part1 $ lines input
        (12,2) -> return $ Day12.part2 $ lines input
        (13,1) -> return $ Day13.part1 $ lines input
        (13,2) -> return $ Day13.part2 $ lines input
        (14,1) -> return $ Day14.part1 $ lines input
        (14,2) -> return $ Day14.part2 $ lines input
        (15,1) -> return $ Day15.part1 $ lines input
        (15,2) -> return $ Day15.part2 $ lines input
        _ -> return (Left "Not yet implemented")