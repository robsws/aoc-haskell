module Day2 (part1, part2) where

roundScore :: (Char, Char) -> Int
roundScore (them, me)
    | me == 'X' && them == 'A' = 1 + 3
    | me == 'X' && them == 'B' = 1 + 0
    | me == 'X' && them == 'C' = 1 + 6
    | me == 'Y' && them == 'A' = 2 + 6
    | me == 'Y' && them == 'B' = 2 + 3
    | me == 'Y' && them == 'C' = 2 + 0
    | me == 'Z' && them == 'A' = 3 + 0
    | me == 'Z' && them == 'B' = 3 + 6
    | me == 'Z' && them == 'C' = 3 + 3
    | otherwise = error ("Unsupported combination of chars.") 

roundScoreOutcome :: (Char, Char) -> Int
roundScoreOutcome (them, outcome)
    | outcome == 'X' && them == 'A' = 3 + 0
    | outcome == 'X' && them == 'B' = 1 + 0
    | outcome == 'X' && them == 'C' = 2 + 0
    | outcome == 'Y' && them == 'A' = 1 + 3
    | outcome == 'Y' && them == 'B' = 2 + 3
    | outcome == 'Y' && them == 'C' = 3 + 3
    | outcome == 'Z' && them == 'A' = 2 + 6
    | outcome == 'Z' && them == 'B' = 3 + 6
    | outcome == 'Z' && them == 'C' = 1 + 6
    | otherwise = error ("Unsupported combination of chars.")

totalScore :: [String] -> ((Char, Char) -> Int) -> Int
totalScore inputs scoreFunc = sum . map scoreFunc . map (\s -> ((head s), (last s))) $ inputs

part1 :: [String] -> String
part1 inputs = show $ totalScore inputs roundScore

part2 :: [String] -> String
part2 inputs = show $ totalScore inputs roundScoreOutcome