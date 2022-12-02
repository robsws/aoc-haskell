import System.Environment

main = do
    args <- getArgs
    input <- getContents
    
    let day :: Int = read (args !! 0)
        part :: Int = read (args !! 1) in

        putStrLn $ case (day, part) of
            (1,1) -> show . maxCalories $ lines input
            _ -> "Not yet implemented"

splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = [[]]
splitByEmpty ([]:strings) = [] : splitByEmpty strings
splitByEmpty (string:strings) =
    let group:groups = splitByEmpty strings in
        (string:group) : groups

maxCalories :: [String] -> Int
maxCalories ls = maximum (map sum . map (map read) $ splitByEmpty ls)