module Day5 (part1, part2, debug) where

import Common (splitBy, transpose)

import Debug.Trace (trace)

data StackCommand = 
    StackMove {
        amount :: Int,
        from   :: Int,
        to     :: Int
    }

parseStackLine :: String -> String
parseStackLine l | trace ("parseStackLine " ++ show l) False = undefined
-- Collapse stack line down to just the characters
-- we need.
parseStackLine "" = ""
parseStackLine l =
    let h = take 4 l
        t = drop 4 l in
            h!!1 : parseStackLine t

parseStacks :: [String] -> [String]
parseStacks ls | trace ("parseStacks " ++ show ls) False = undefined  
-- Parse stack lines into n strings each
-- representing one of the stacks from top
-- to bottom.
parseStacks ls =
    let transposed = map parseStackLine ls in
        map (dropWhile (== ' ')) (transpose transposed)

parseCommand :: String -> StackCommand
-- Convert formatted string into StackCommand record.
parseCommand s =
    let ws = words s in
        -- Minus 1 from indices because they are 1-indexed
        StackMove {
            amount = read (ws!!1),
            from   = read (ws!!3) - 1, 
            to     = read (ws!!5) - 1
        }

takeCrate :: [String] -> Int -> (Char, [String])
-- Pop a crate off the specified stack and return the crate and the stacks
takeCrate stacks fromIndex | trace ("takeCrate " ++ show stacks ++ " " ++ show fromIndex) False = undefined  
takeCrate stacks fromIndex
    | fromIndex >= length stacks = error ("Index " ++ show fromIndex ++ " out of bounds. Amount of stacks: " ++ show (length stacks))
    | fromIndex == 0 = (head stack, ((tail stack) : tail stacks))
    | otherwise = 
        let (crate, stacks') = takeCrate (tail stacks) (fromIndex-1) in
            (crate, stack : stacks')
    where stack = head stacks

putCrate :: [String] -> Char -> Int -> [String]
putCrate stacks crate toIndex | trace ("putCrate " ++ show stacks ++ " " ++ show crate ++ " " ++ show toIndex) False = undefined  
-- Push a crate on to the specified stack and return the stacks
putCrate stacks crate toIndex
    | toIndex == 0 = (crate : stack) : tail stacks
    | otherwise = 
        let stacks' = putCrate (tail stacks) crate (toIndex-1) in
            stack:stacks'
    where stack = head stacks

moveCrate :: [String] -> Int -> Int -> [String]
-- Move a crate from one stack to another
moveCrate stacks fromIndex toIndex =
    let (crate, stacks') = takeCrate stacks fromIndex in
        putCrate stacks' crate toIndex

parseInput :: [String] -> ([String], [StackCommand])
-- Parse entire input file to a list of stacks and a list of commands
parseInput ls =
    let parts = splitBy "" ls in
        (
            parseStacks $ init $ parts!!0,
            map parseCommand $ parts!!1
        )

executeCommands :: [StackCommand] -> [String] -> [String]
-- Move crates about on the stack according to the commands
executeCommands [] stacks = stacks
executeCommands (StackMove{amount=a,from=fi,to=ti}:commands) stacks
    | a > 0 = 
        let stacks' = moveCrate stacks fi ti 
            command' = StackMove{amount=(a - 1),from=fi,to=ti} in
            executeCommands (command':commands) stacks'
    | otherwise =
            executeCommands commands stacks

part1 :: [String] -> String
part1 inputs = 
    let (stacks, commands) = parseInput inputs in
        map head $ executeCommands commands stacks

part2 :: [String] -> String
part2 inputs = ""

debug :: [String] -> String
debug inputs = ""