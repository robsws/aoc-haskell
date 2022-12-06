module Day5 (part1, part2) where

import Common (splitBy, transpose)

data StackCommand = 
    MoveOneByOne {amount :: Int, from :: Int, to :: Int} |
    MoveAllAtOnce {amount :: Int, from :: Int, to :: Int}

parseStackLine :: String -> String
-- Collapse stack line down to just the characters
-- we need.
parseStackLine "" = ""
parseStackLine l =
    let h = take 4 l
        t = drop 4 l in
            h!!1 : parseStackLine t

parseStacks :: [String] -> [String]
-- Parse stack lines into n strings each
-- representing one of the stacks from top
-- to bottom.
parseStacks ls =
    let transposed = map parseStackLine ls in
        map (dropWhile (== ' ')) (transpose transposed)

parseCommand :: Bool -> String -> StackCommand
-- Convert formatted string into StackCommand record.
parseCommand oneByOne s =
    let ws = words s in
        -- Minus 1 from indices because they are 1-indexed
        if oneByOne then
            MoveOneByOne {
                amount = read (ws!!1),
                from   = read (ws!!3) - 1, 
                to     = read (ws!!5) - 1
            }
        else
            MoveAllAtOnce {
                amount = read (ws!!1),
                from   = read (ws!!3) - 1, 
                to     = read (ws!!5) - 1
            }

takeCrates :: [String] -> Int -> Int -> (String, [String])
-- Pop a crate off the specified stack and return the crate and the stacks
takeCrates stacks noOfCrates fromIndex
    | fromIndex >= length stacks = error ("Index " ++ show fromIndex ++ " out of bounds. Amount of stacks: " ++ show (length stacks))
    | fromIndex == 0 = (take noOfCrates stack, ((drop noOfCrates stack) : tail stacks))
    | otherwise = 
        let (crate, stacks') = takeCrates (tail stacks) noOfCrates (fromIndex-1) in
            (crate, stack : stacks')
    where stack = head stacks

putCrates :: [String] -> String -> Int -> [String]
-- Push a crate on to the specified stack and return the stacks
putCrates stacks crates toIndex
    | toIndex == 0 = (crates ++ stack) : tail stacks
    | otherwise = 
        let stacks' = putCrates (tail stacks) crates (toIndex-1) in
            stack:stacks'
    where stack = head stacks

moveCrates :: [String] -> Int -> Int -> Int -> [String]
-- Move crates from one stack to another maintaining order
moveCrates stacks noOfCrates fromIndex toIndex =
    let (crates, stacks') = takeCrates stacks noOfCrates fromIndex in
        putCrates stacks' crates toIndex

parseInput :: Bool -> [String] -> ([String], [StackCommand])
-- Parse entire input file to a list of stacks and a list of commands
parseInput oneByOne ls =
    let parts = splitBy "" ls in
        (
            parseStacks $ init $ parts!!0,
            map (parseCommand oneByOne) $ parts!!1
        )

executeCommands :: [StackCommand] -> [String] -> [String]
-- Move crates about on the stack according to the commands
executeCommands [] stacks = stacks
executeCommands (MoveOneByOne{amount=a,from=fi,to=ti}:commands) stacks
    | a > 0 = 
        let stacks' = moveCrates stacks 1 fi ti 
            command' = MoveOneByOne{amount=(a - 1),from=fi,to=ti} in
            executeCommands (command':commands) stacks'
    | otherwise =
            executeCommands commands stacks
executeCommands (MoveAllAtOnce{amount=a,from=fi,to=ti}:commands) stacks =
    let stacks' = moveCrates stacks a fi ti in
        executeCommands commands stacks'

part1 :: [String] -> String
part1 inputs = 
    let (stacks, commands) = parseInput True inputs in
        map head $ executeCommands commands stacks

part2 :: [String] -> String
part2 inputs = 
    let (stacks, commands) = parseInput False inputs in
        map head $ executeCommands commands stacks