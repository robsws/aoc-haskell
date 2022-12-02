import System.Environment

import qualified Day1 (part1, part2)

main = do
    args <- getArgs
    input <- getContents

    let day :: Int = read (args !! 0)
        part :: Int = read (args !! 1) in

        putStrLn $ case (day, part) of
            (1,1) -> Day1.part1 $ lines input
            (1,2) -> Day1.part2 $ lines input
            _ -> "Not yet implemented"