module Day10 (part1, part2) where

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
part2 inputs = ""