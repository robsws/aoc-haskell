module Day3 (part1, part2) where

import Data.Char (ord)
import Data.Maybe (fromJust)

import Common (pair2list, chunksOf)

priority :: Char -> Int
-- a - z = 1  - 26
-- A - Z = 27 - 52
priority c
    | ascii >= 97 && ascii <= 122 = ascii - 96
    | ascii >= 65 && ascii <= 90 = ascii - 38
    | otherwise = error ("Cannot get priority for " ++ [c])
    where ascii = ord c

splitIntoHalves :: String -> (String, String)
-- e.g. abcdef = (abc, def)
splitIntoHalves s = let half = (length s) `div` 2 in
    (take half s, drop half s)

getCommonLetter :: [String] -> Maybe Char
-- loop over characters in first string and
-- look for them in other strings
getCommonLetter [] = error ("No strings to compare.") 
getCommonLetter ("":_) = Nothing
getCommonLetter ((needle:rest):haystacks)
    | foldr (&&) True (map (needle `elem`) haystacks) = Just needle
    | otherwise = getCommonLetter (rest:haystacks)

part1 :: [String] -> String
part1 inputs =
    show . -- convert number to string
    sum . -- add up all the priorities
    map priority . -- calculate the priority of the item
    map fromJust . -- make sure that there was a common item
    map getCommonLetter . -- get each item common to both halves
    map pair2list . -- convert pair to list
    map splitIntoHalves -- split each string into two halves
        $ inputs

part2 :: [String] -> String
part2 inputs =
    show . -- convert to number
    sum . -- add up all priorities
    map priority . -- calculate priority of each item
    map fromJust . -- make sure that there was a common item
    map getCommonLetter . -- get the common letter in each group
    chunksOf 3 -- split input lines into groups of three
        $ inputs