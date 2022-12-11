module Day10 (part1, part2) where

import Data.List (intercalate)

import Common (chunksOf)

data Instruction = Addx Int | Noop deriving (Show)

parseProgram :: [String] -> [Instruction]
parseProgram [] = []
parseProgram (hLine:tLines)
    | name == "addx" = Noop : (Addx (read (argv!!1))) : parseProgram tLines
    | name == "noop" =                           Noop : parseProgram tLines
    | otherwise = error ("Invalid instruction name: " ++ name)
    where argv = words hLine
          name = argv!!0

runProgram :: [Instruction] -> Int -> [Int]
runProgram [] _ = []
runProgram ((Addx val):tInsts) x =
    x:(runProgram tInsts (x+val))
runProgram (Noop:tInsts) x = x:(runProgram tInsts x)

signalStrength :: Int -> [Int] -> Int
signalStrength t xHistory = t * (xHistory!!(t-1))

drawCrtImpl :: [Int] -> Int -> String
drawCrtImpl [] _ = ""
drawCrtImpl (x:xHistory) t
    | crtX `elem` [(x-1) .. (x+1)] = '#':remainingCrt
    | otherwise = '.':remainingCrt
    where crtX = t `mod` 40
          remainingCrt = drawCrtImpl xHistory (t+1)

drawCrt :: [Int] -> String
drawCrt xHistory = intercalate "\n" . chunksOf 40 $ drawCrtImpl xHistory 0

part1 :: [String] -> String
part1 inputs =
    let xHistory = runProgram (parseProgram inputs) 1
        in show $ (signalStrength 20 xHistory) +
                  (signalStrength 60 xHistory) +
                  (signalStrength 100 xHistory) +
                  (signalStrength 140 xHistory) +
                  (signalStrength 180 xHistory) +
                  (signalStrength 220 xHistory)

part2 :: [String] -> String
part2 inputs =
    let xHistory = runProgram (parseProgram inputs) 1
        in drawCrt xHistory