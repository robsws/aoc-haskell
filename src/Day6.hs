module Day6 (part1, part2) where

allCharsDifferent :: String -> Bool
allCharsDifferent "" = True
allCharsDifferent (c:s)
    | c `elem` s = False
    | otherwise = allCharsDifferent s

findMarker :: String -> Int -> Int
findMarker stream markerLen
    | allCharsDifferent $ take markerLen stream = markerLen
    | otherwise = 1 + findMarker (tail stream) markerLen

part1 :: String -> String
part1 input = show $ findMarker input 4

part2 :: String -> String
part2 input = show $ findMarker input 14