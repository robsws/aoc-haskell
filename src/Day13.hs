module Day13 (part1, part2) where

import Data.List ( elemIndices, elemIndex, sort )
import Control.Monad.Except (throwError)

import Common ( splitBy )

data PacketToken = OpenListTok | CloseListTok | ValueTok Int deriving (Show)
data Packet = List [Packet] | Value Int deriving (Eq, Show)

instance Ord Packet where
    -- compare x y | trace ("comp: " ++ (show x) ++ " " ++ (show y)) False = undefined
    compare (Value v1) (Value v2) = compare v1 v2
    compare (List []) (List []) = EQ
    compare (List []) (List (_:_)) = LT
    compare (List (_:_)) (List []) = GT
    compare (List (p1:ps1)) (List (p2:ps2)) = 
        case compare p1 p2 of
            EQ -> compare ps1 ps2
            o -> o
    compare ps@(List _) v@(Value _) = compare ps (List [v])
    compare v@(Value _) ps@(List _) = compare (List [v]) ps

lexInput :: String -> [PacketToken]
lexInput "" = []
lexInput (ch:chs) =
    case ch of
        '[' -> OpenListTok : lexInput chs
        ']' -> CloseListTok : lexInput chs
        ',' -> lexInput chs
        _ -> let tok = ch : takeWhile notSymbol chs
                 rest = dropWhile notSymbol chs
             in ValueTok (read tok) : lexInput rest
    where notSymbol c = not $ c `elem` "[],"

parsePacket :: [Packet] -> [PacketToken] -> (Packet, [PacketToken])
parsePacket acc [] = (head acc, [])
parsePacket acc (CloseListTok : toks) = (List (reverse acc), toks)
parsePacket acc (OpenListTok : toks) =
    let (innerPacketList, toks') = parsePacket [] toks
    in parsePacket (innerPacketList : acc) toks'
parsePacket acc (ValueTok val : toks) = parsePacket (Value val : acc) toks

part1 :: [String] -> Either String String
part1 inputs = 
    let pairs = splitBy [] inputs
        packetPairs = map (map (fst . parsePacket [] . lexInput)) pairs
    in do
        if any (\p -> length p /= 2) packetPairs then
            throwError "Amount of packets in group /= 2"
        else
            let results = map (\p -> (p!!0) <= (p!!1)) packetPairs
            in return $ show . sum $ map (+1) (elemIndices True results)

part2 :: [String] -> Either String String
part2 inputs =
    let packetStrs = filter (/="") inputs
        packets = map (fst . parsePacket [] . lexInput) packetStrs
        dividers = [List [Value 2], List [Value 6]]
        sorted = sort (packets ++ dividers)
        indices = map (\d -> fmap (+1) (elemIndex d sorted)) dividers
        result = fmap product $ sequence indices
    in 
        case result of
            Nothing -> Left "Dividers went missing after the sort"
            Just val -> Right $ show $ val

