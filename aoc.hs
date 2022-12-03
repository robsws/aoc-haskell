import System.Environment

import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)

main = do
    args <- getArgs
    input <- getContents

    let day :: Int = read (args !! 0)
        part :: Int = read (args !! 1) in

        putStrLn $ case (day, part) of
            (1,1) -> Day1.part1 $ lines input
            (1,2) -> Day1.part2 $ lines input
            (2,1) -> Day2.part1 $ lines input
            (2,2) -> Day2.part2 $ lines input
            (3,1) -> Day3.part1 $ lines input
            (3,2) -> Day3.part2 $ lines input
            _ -> "Not yet implemented"