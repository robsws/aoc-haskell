module Day3 (part1, part2) where

import Data.Char
import Data.List

priority :: Char -> Int
-- a - z = 1  - 26
-- A - Z = 27 - 52
priority c
    | ascii >= 97 && ascii <= 122 = ascii - 96
    | ascii >= 65 && ascii <= 90 = ascii - 38
    where ascii = ord c

splitIntoHalves :: String -> (String, String)
-- e.g. abcdef = (abc, def)
splitIntoHalves s = let half = (length s) `div` 2 in
    (take half s, drop half s)

getCommonLetter :: [String] -> Char
-- loop over characters in first string and
-- look for them in other strings
getCommonLetter ("":haystacks) = '-'
getCommonLetter ((needle:rest):haystacks)
    | foldr (&&) True (map (needle `elem`) haystacks) = needle
    | otherwise = getCommonLetter (rest:haystacks)

chunksOf :: Int -> [a] -> [[a]]
-- split a list into sublists of length n
chunksOf n [] = []
chunksOf n ls = take n ls : chunksOf n (drop n ls)

pair2list :: (a,a) -> [a]
-- convert a pair into a two element list
pair2list (x,y) = [x,y]

part1 :: [String] -> String
part1 inputs =
    show . -- convert number to string
    sum . -- add up all the priorities
    map priority . -- calculate the priority of the item
    map getCommonLetter . -- get each item common to both halves
    map pair2list . -- convert pair to list
    map splitIntoHalves -- split each string into two halves
        $ inputs

part2 :: [String] -> String
part2 inputs =
    show . -- convert to number
    sum . -- add up all priorities
    map priority . -- calculate priority of each item
    map getCommonLetter . -- get the common letter in each group
    chunksOf 3 -- split input lines into groups of three
        $ inputs