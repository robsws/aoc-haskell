module Day15 (part1, part2) where

import Debug.Trace ( trace )
import Data.Set (Set)
import qualified Data.Set as Set

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

part1 :: [String] -> Either String String
part1 inputs = 
    case (sequence $ map parseLine inputs) of
        Nothing -> Left "Parsing error"
        Just sensorInfos -> let maxX = maximum $ map (\s -> (getX.getSensor) s + getRange s) sensorInfos
                                minX = minimum $ map (\s -> (getX.getSensor) s - getRange s) sensorInfos
                                beacons = Set.fromList $ map getBeacon sensorInfos
                            in Right $ show $ coveredOnRow sensorInfos beacons maxX minX 2000000 0

part2 :: [String] -> Either String String
part2 inputs = Left ""