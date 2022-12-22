module Day15 (part1, part2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
    ( MonadState(get, state), runState, State )
import Data.List ( sort, find )
import Data.Maybe ( isJust )

import Parsing ( getAllIntsOnLine )
import Common (getElemAt)
import Coord ( Coord(..) )
import Grid (manhattan)

data SensorInfo = SensorInfo { getSensor :: Coord, getBeacon :: Coord, getRange :: Int }

parseLine :: String -> Maybe SensorInfo
parseLine l =
    let nums = getAllIntsOnLine l
    in do
        sensorX <- getElemAt nums 0
        sensorY <- getElemAt nums 1
        beaconX <- getElemAt nums 2
        beaconY <- getElemAt nums 3
        return $ let sensor = Coord sensorX sensorY
                     beacon = Coord beaconX beaconY
                     range = manhattan sensor beacon
                 in SensorInfo sensor beacon range

notBeacon :: Coord -> SensorInfo -> Bool
notBeacon loc sensorInfo = manhattan loc (getSensor sensorInfo) <= (getRange sensorInfo)

coveredOnRow :: [SensorInfo] -> Set Coord -> Int -> Int -> Int -> Int -> Int
coveredOnRow sensorInfos beacons maxX x y acc
    | x >= maxX = acc
    | otherwise =
        let point = Coord x y
            notInBeacons = Set.notMember point beacons
            coveredBySensor = any (\s -> notBeacon point s) sensorInfos
            notABeacon = notInBeacons && coveredBySensor
        in if notABeacon then
            coveredOnRow sensorInfos beacons maxX (x+1) y (acc+1)
        else
            coveredOnRow sensorInfos beacons maxX (x+1) y acc

addRange :: (Int, Int) -> State [(Int, Int)] ()
addRange r = state $ \s -> ((), r:s)

getSensorRangeOnRow :: SensorInfo -> Int -> Maybe (Int, Int)
getSensorRangeOnRow (SensorInfo (Coord sx sy) _ range) y =
    let remainderForX = range - abs (sy - y)
    in if remainderForX > 0 then
           Just (sx-remainderForX, sx+remainderForX)
       else
           Nothing

findGapInRanges :: [(Int, Int)] -> Int -> Int -> Int -> Maybe Coord
findGapInRanges [] _ _ _ = Nothing
findGapInRanges ((s, e):rs) minX maxX y
    | s > (minX+1) = Just $ Coord (minX+1) y
    | otherwise = findGapInRanges rs (max minX e) maxX y

gapOnRow :: [SensorInfo] -> Int -> Int -> State [(Int, Int)] (Maybe Coord)
gapOnRow [] y maxX = do
    ranges <- get
    return $ findGapInRanges (sort ranges) 0 maxX y
gapOnRow (sensorInfo:sensorInfos) y maxX = do
    case getSensorRangeOnRow sensorInfo y of
        Nothing -> return ()
        Just range -> addRange range
    gapOnRow sensorInfos y maxX

part1 :: [String] -> Either String String
part1 inputs = 
    case (sequence $ map parseLine inputs) of
        Nothing -> Left "Parsing error"
        Just sensorInfos -> let maxX = maximum $ map (\s -> (getX.getSensor) s + getRange s) sensorInfos
                                minX = minimum $ map (\s -> (getX.getSensor) s - getRange s) sensorInfos
                                beacons = Set.fromList $ map getBeacon sensorInfos
                            in Right $ show $ coveredOnRow sensorInfos beacons maxX minX 2000000 0

part2 :: [String] -> Either String String
part2 inputs = 
    case (sequence $ map parseLine inputs) of
        Nothing -> Left "Parsing error"
        Just sensorInfos ->
            let maxX = 4000000
                gaps = map (\y -> fst $ runState (gapOnRow sensorInfos y maxX) []) [0..maxX]
                gap = find isJust gaps
            in case gap of
                Nothing -> Left "No gap found."
                Just (Just (Coord gx gy)) -> return $ show $ gx * 4000000 + gy
                _ -> error ("Somehow ended up with no Just gap passing isJust")
