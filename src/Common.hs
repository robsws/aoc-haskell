module Common (
    splitBy,
    pair2list
) where

splitBy :: (Eq a) => a -> [a] -> [[a]]
-- Split a list by delimiter.
splitBy _ [] = [[]]
splitBy delimiter (h:t)
    | h == delimiter = [] : splitTail
    | otherwise = (h:(head splitTail)) : tail splitTail
    where splitTail = splitBy delimiter t

pair2list :: (a,a) -> [a]
-- convert a pair into a two element list
pair2list (x,y) = [x,y] 