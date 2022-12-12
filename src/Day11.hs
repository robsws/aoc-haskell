module Day11 (part1, part2) where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Maybe (fromJust)
import Debug.Trace

import Common (splitBy)

data MonkeyRules = MonkeyRules (Int -> Int) (Int -> Bool) Int Int

data Monkey = Monkey [Int] MonkeyRules Int

instance Show Monkey where
    show (Monkey items _ inspectCount) = (show items) ++ (show inspectCount)

instance Eq Monkey where
    (Monkey _ _ inspectCountA) == (Monkey _ _ inspectCountB) = inspectCountA == inspectCountB

instance Ord Monkey where
    compare (Monkey _ _ inspectCountA) (Monkey _ _ inspectCountB) =
        compare inspectCountA inspectCountB

type MonkeyCircle = Seq Monkey

parseInputs :: [[String]] -> [Monkey]
parseInputs [] = []
parseInputs (monkeyDef:monkeyDefs) =
    monkey:(parseInputs monkeyDefs)
    where monkeyDef' = map (\l -> (splitBy ':' l)!!1) monkeyDef
          sItems = filter (\c -> c /= ',') (monkeyDef'!!1)
          sItems' = words sItems
          items = map read sItems'
          sOp = words (monkeyDef'!!2)
          op = parseOp sOp
          divisor = read ((words (monkeyDef'!!3))!!2)
          test = (\v -> v `mod` divisor == 0)
          trueMonkey = read ((words (monkeyDef'!!4))!!3)
          falseMonkey = read ((words (monkeyDef'!!5))!!3)
          monkey = (Monkey items (MonkeyRules op test trueMonkey falseMonkey) 0)

parseOp :: [String] -> (Int -> Int)
parseOp opParts
    | length opParts /= 5 = error ("Invalid operation.")
    | a == "old" && binop == "*" && b == "old" = (\x -> x * x)
    | a == "old" && binop == "+" && b == "old" = (\x -> x + x)
    | a == "old" && binop == "*" = (\x -> x * (read b))
    | a == "old" && binop == "+" = (\x -> x + (read b))
    | otherwise = error ("Unsupported operation")
    where a = opParts!!2
          b = opParts!!4
          binop = opParts!!3

inspectItem :: Int -> MonkeyRules -> (Int, Int)
inspectItem item (MonkeyRules op test trueI falseI)
    | test item' = (item', trueI)
    | otherwise = (item', falseI)
    where item' = (op item) `div` 3

throwItems :: Monkey -> [(Int, Int)]
throwItems (Monkey [] _ _) = []
throwItems (Monkey (item:items) rules inspectCount) =
    (item', throwTo) : (throwItems monkey')
    where (item', throwTo) = inspectItem item rules
          monkey' = Monkey items rules inspectCount

catchItems :: [(Int, Int)] -> MonkeyCircle -> MonkeyCircle
catchItems [] monkeys = monkeys
catchItems ((item, i):throws) monkeys = catchItems throws monkeys'
    where monkey = fromJust (S.lookup i monkeys)
          (Monkey items rules inspectCount) = monkey
          monkey' = Monkey (item:items) rules inspectCount
          monkeys' = S.update i monkey' monkeys

doMonkeyTurn :: MonkeyCircle -> Int -> MonkeyCircle
doMonkeyTurn monkeys i = monkeys''
    where monkey = fromJust (S.lookup i monkeys)
          (Monkey _ rules inspectCount) = monkey
          thrownItems = throwItems monkey
          inspectCount' = inspectCount + (length thrownItems)
          monkey' = (Monkey [] rules inspectCount')
          monkeys' = S.update i monkey' monkeys
          monkeys'' = catchItems thrownItems monkeys'

doRound :: MonkeyCircle -> MonkeyCircle
doRound monkeys = foldl doMonkeyTurn monkeys [0 .. (length monkeys - 1)]

monkeyBusiness :: MonkeyCircle -> Int -> Int
-- monkeyBusiness monkeys rounds | trace ((show rounds) ++ " " ++ (show monkeys)) False = undefined
monkeyBusiness monkeys rounds
    | rounds == 0 =
        foldl (\acc (Monkey _ _ v) -> acc * v) 1 .
        S.take 2 .
        S.reverse .
        S.sort $ monkeys
    | otherwise =
        let monkeys' = doRound monkeys
            in monkeyBusiness monkeys' (rounds-1)

part1 :: [String] -> String
part1 inputs =
    let monkeys = S.fromList . parseInputs $ splitBy "" inputs 
        in show $ monkeyBusiness monkeys 20

part2 :: [String] -> String
part2 inputs = ""