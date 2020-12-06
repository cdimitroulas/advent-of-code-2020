module Day6 where

import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

getNumberOfYesAnswers :: String -> Int
getNumberOfYesAnswers = sum . map (length . Set.fromList . concat . lines) . splitOn "\n\n"

part1 :: IO ()
part1 = readFile "lib/day6.txt" >>= print . getNumberOfYesAnswers

-- Given a list of strings like ["abc", "a", "b"] count the occurrences of each character
-- Create a Map with number of occurrences of each char. Then check
-- how many keys in the map are equal to the length of the list (indicated all strings in list
-- had that char)
getNoOfQsEver1AnsweredYes :: [String] -> Int
getNoOfQsEver1AnsweredYes list = Map.size . Map.filter (== length list). Map.unionsWith (+) . map (Map.fromSet (const (1 :: Int)) . Set.fromList) $ list
-- thing = foldl (\accum str -> Map.insertWith (++) 1 x) Map.empty

getNumberOfYesAnswers' :: String -> Int
getNumberOfYesAnswers' = sum . map (getNoOfQsEver1AnsweredYes . lines) . splitOn "\n\n"

part2 :: IO ()
part2 = readFile "lib/day6.txt" >>= print . getNumberOfYesAnswers'
