module Day10 where

import Data.List
import Text.Read

parseInput :: String -> Maybe [Int]
parseInput = mapM (\x -> readMaybe x :: Maybe Int) . lines

getDifferences :: [Int] -> [Int]
getDifferences l = zipWith (flip (-)) fullList (tail fullList)
  where
    sortedL = sort l
    fullList = 0 : sortedL ++ [last sortedL + 3]

countOccurrence :: Eq a => a -> [a] -> Int
countOccurrence x = length . filter (== x)

part1 :: IO ()
part1 = do
  inputM <- parseInput <$> readFile "lib/day10.txt"
  print $ do
    (\x -> countOccurrence 1 x * countOccurrence 3 x) . getDifferences <$> inputM
