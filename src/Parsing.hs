module Parsing (
    getAllIntsOnLine
) where

getAllIntsOnLine :: String -> [Int]
getAllIntsOnLine [] = []
getAllIntsOnLine (ch:line)
    | isNumChar ch = 
        let num = takeWhile isNumChar (ch:line)
            rest = dropWhile isNumChar line
        in read num : getAllIntsOnLine rest
    | ch == '-' && isNumChar (head line) = 
        let num = takeWhile isNumChar line
            rest = dropWhile isNumChar line
        in negate (read num) : getAllIntsOnLine rest
    | otherwise = getAllIntsOnLine line
    where isNumChar c = c `elem` "0123456789"