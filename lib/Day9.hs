module Day9 where

import Text.Read (readMaybe)

addT :: (Int, Int) -> Int
addT = uncurry (+)

readInt :: String -> Maybe Int
readInt = readMaybe

findFirstException :: Int -> [Int] -> Int
findFirstException _ [] = error "Nope"
findFirstException preamble l =
  if any (\x -> addT x == elemToCheck) pairs
    then findFirstException preamble (tail l)
    else elemToCheck
  where
    subList = take (preamble + 1) l
    elemToCheck = last subList
    pairs = [(a, b) | a <- subList, b <- subList, a /= b]

part1 :: IO ()
part1 = do
  inputm <- traverse readInt . lines <$> readFile "lib/day9.txt"
  print $ findFirstException 25 <$> inputm
