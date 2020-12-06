module Day5 (getSeatId, part1, part2) where

import Data.List (sort)

-- Part 1

data SearchOperator = UpperHalf | LowerHalf deriving (Eq, Show)

binarySearch :: [SearchOperator] -> [Int] -> Int
binarySearch _ [x] = x
binarySearch (searchOp:sos) list
  | searchOp == LowerHalf = binarySearch sos $ take (length list `div` 2) list
  | searchOp == UpperHalf = binarySearch sos $ drop (length list `div` 2) list
binarySearch _ _ = error "Invalid arguments"

-- Yeah, I know, I'm lazy and assume that we will only get the right characters in these Strings
-- (either "F"/"B" or "L"/"R")
mkRowSearchOperators :: String -> [SearchOperator]
mkRowSearchOperators = map (\x -> if x == 'F' then LowerHalf else UpperHalf)

mkColSearchOperators :: String -> [SearchOperator]
mkColSearchOperators = map (\x -> if x == 'L' then LowerHalf else UpperHalf)

getSeatId :: String -> Int
getSeatId seatCode = rowNo * 8 + colNo
  where
    (rowCode, columnCode) = splitAt 7 seatCode
    rowNo = binarySearch (mkRowSearchOperators rowCode) [0..127]
    colNo = binarySearch (mkColSearchOperators columnCode) [0..7]

part1 :: IO ()
part1 = readFile "lib/day5.txt" >>= print . maximum . map getSeatId . lines

-- Part 2

getMissingSeatId :: [Int] -> Int
getMissingSeatId [] = error "Empty list passed to getMissingSeatId"
getMissingSeatId [_] = error "did not find result"
getMissingSeatId [_, _] = error "did not find result"
getMissingSeatId list = 
  if x1 + 1 == x2 && x2 + 1 == x3
     then getMissingSeatId (x2:x3:xs)
     else x3 - 1
  where
    (x1:x2:x3:xs) = sort list


part2 :: IO ()
part2 = readFile "lib/day5.txt" >>= print . getMissingSeatId . map getSeatId . lines
