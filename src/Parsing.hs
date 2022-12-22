module Parsing (
    getAllIntsOnLine
) where

getAllIntsOnLine :: String -> [Int]
getAllIntsOnLine [] = []
getAllIntsOnLine (ch:line)
    | isNumChar ch = read num : getAllIntsOnLine rest
    | otherwise = getAllIntsOnLine line
    where isNumChar c = c `elem` "0123456789"
          num = takeWhile isNumChar (ch:line)
          rest = dropWhile isNumChar line