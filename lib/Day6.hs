module Day6 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

getNumberOfYesAnswers :: String -> Int
getNumberOfYesAnswers = sum . map (length . Set.fromList . concat . lines) . splitOn "\n\n"

part1 :: IO ()
part1 = readFile "lib/day6.txt" >>= print . getNumberOfYesAnswers

countCharOccurrences :: [String] -> Map Char Int
countCharOccurrences =
  Map.unionsWith (+)
  . map (Map.fromSet (const (1 :: Int)) . Set.fromList)

getNoOfQsEver1AnsweredYes :: [String] -> Int
getNoOfQsEver1AnsweredYes list =
    Map.size
  . Map.filter (== length list)
  . countCharOccurrences
  $ list

getNumberOfYesAnswers' :: String -> Int
getNumberOfYesAnswers' = sum . map (getNoOfQsEver1AnsweredYes . lines) . splitOn "\n\n"

part2 :: IO ()
part2 = readFile "lib/day6.txt" >>= print . getNumberOfYesAnswers'
