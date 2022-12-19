module Day11 (part1, part2) where

import Data.Sequence (Seq)
import Data.Foldable
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

import Common (splitBy)

data MonkeyRules = MonkeyRules (Int -> Int) Int Int Int

data Monkey = Monkey [Int] MonkeyRules Int

instance Show Monkey where
    show (Monkey items _ inspectCount) = (show items) ++ " " ++ (show inspectCount)

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
          trueMonkey = read ((words (monkeyDef'!!4))!!3)
          falseMonkey = read ((words (monkeyDef'!!5))!!3)
          monkey = (Monkey items (MonkeyRules op divisor trueMonkey falseMonkey) 0)

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

multiplyDivisors :: MonkeyCircle -> Int
multiplyDivisors monkeys =
    foldr (*) 1 (map f (toList monkeys))
    where f = (\(Monkey _ (MonkeyRules _ divisor _ _) _) -> divisor)

inspectItem :: Int -> MonkeyRules -> Bool -> Int -> (Int, Int)
inspectItem item (MonkeyRules op divisor trueI falseI) div3 maxToStore
    | item' `mod` divisor == 0 = (item', trueI)
    | otherwise = (item', falseI)
    where item' = if div3 then
                      ((op item) `div` 3) -- `mod` maxToStore
                  else
                      (op item) `mod` maxToStore
          

throwItems :: Monkey -> Bool -> Int -> [(Int, Int)]
throwItems (Monkey [] _ _) _ _ = []
throwItems (Monkey (item:items) rules inspectCount) div3 maxToStore =
    (item', throwTo) : (throwItems monkey' div3 maxToStore)
    where (item', throwTo) = inspectItem item rules div3 maxToStore
          monkey' = Monkey items rules inspectCount

catchItems :: [(Int, Int)] -> MonkeyCircle -> MonkeyCircle
catchItems [] monkeys = monkeys
catchItems ((item, i):throws) monkeys = catchItems throws monkeys'
    where monkey = fromJust (S.lookup i monkeys)
          (Monkey items rules inspectCount) = monkey
          monkey' = Monkey (item:items) rules inspectCount
          monkeys' = S.update i monkey' monkeys

doMonkeyTurn :: Int -> Bool -> MonkeyCircle -> Int -> MonkeyCircle
doMonkeyTurn maxToStore div3 monkeys i = monkeys''
    where monkey = fromJust (S.lookup i monkeys)
          (Monkey _ rules inspectCount) = monkey
          thrownItems = throwItems monkey div3 maxToStore
          inspectCount' = inspectCount + (length thrownItems)
          monkey' = (Monkey [] rules inspectCount')
          monkeys' = S.update i monkey' monkeys
          monkeys'' = catchItems thrownItems monkeys'

doRound :: Int -> Bool -> MonkeyCircle -> MonkeyCircle
doRound maxToStore div3 monkeys =
    foldl (doMonkeyTurn maxToStore div3) monkeys [0 .. (length monkeys - 1)]

monkeyBusiness :: MonkeyCircle -> Int -> Int -> Bool -> Int
monkeyBusiness monkeys rounds maxToStore div3
    | rounds == 0 =
        foldl (\acc (Monkey _ _ v) -> acc * v) 1 .
        S.take 2 .
        S.reverse .
        S.sort $ monkeys
    | otherwise =
        let monkeys' = doRound maxToStore div3 monkeys
            in monkeyBusiness monkeys' (rounds-1) maxToStore div3

solve :: [String] -> Int -> Bool -> Int
solve inputs rounds div3 =
    let monkeys = S.fromList . parseInputs $ splitBy "" inputs
        maxToStore = multiplyDivisors monkeys 
        in monkeyBusiness monkeys rounds maxToStore div3

part1 :: [String] -> String
part1 inputs = show $ solve inputs 20 True

part2 :: [String] -> String
part2 inputs = show $ solve inputs 10000 False