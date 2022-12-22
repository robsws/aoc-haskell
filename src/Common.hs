module Common (
    splitBy,
    pair2list,
    transpose,
    chunksOf,
    window,
    getElemAt
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

transpose :: (Eq a, Show a) => [[a]] -> [[a]]
-- Transpose a 2D list like a matrix.
transpose ll 
    -- if all sublists are empty, don't recurse any further
    | foldr (&&) True (map (== []) ll) = []
    | otherwise = (map head ll) : (transpose $ map tail ll)

chunksOf :: Int -> [a] -> [[a]]
-- split a list into sublists of length n
chunksOf _ [] = []
chunksOf n ls = take n ls : chunksOf n (drop n ls)

window :: Int -> [a] -> [[a]]
-- iterate over sliding windows of the list
window size l
    | length l <= size = [l]
    | otherwise = take size l : window size (tail l)

getElemAt :: [a] -> Int -> Maybe a
-- get an item at the given index in a list, or Nothing if the index is out of bounds
getElemAt (e:_) 0 = Just e
getElemAt l i
    | i < 0 || i >= length l = Nothing
    | otherwise = getElemAt (tail l) (i-1)