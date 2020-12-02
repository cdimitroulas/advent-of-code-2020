module Day1 (main) where

import Control.Applicative

-- Report Repair
type ExpenseReport = [Int]

-- Go through the list one element at a time
--
-- Using each element, add it with all others in the list and check if the result
-- is equal to 2020
--
-- Return the two elements which are equal to 2020

-- Creates all possible combinations of pairs from a list
getPairCombos :: [a] -> [(a, a)]
getPairCombos xs = liftA2 (,) xs xs

-- Creates all possible combinations of triples from a list
getTripleCombos :: [a] -> [(a, a, a)]
getTripleCombos xs = liftA3 (,,) xs xs xs

pairSumIs2020 :: (Int, Int) -> Bool
pairSumIs2020 = (==) 2020 . uncurry (+)

tripleOperation :: (Int -> Int -> Int) -> (Int, Int, Int) -> Int
tripleOperation f (a,b,c) = f (f a b) c

sumTriple = tripleOperation (+)

tripleSumIs2020 = (==) 2020 . sumTriple

findRightCombo :: [(Int, Int)] -> (Int, Int)
findRightCombo (x:xs) = if pairSumIs2020 x then x else findRightCombo xs

findRightTriple :: [(Int, Int, Int)] -> (Int, Int, Int)
findRightTriple (x:xs) = if tripleSumIs2020 x then x else findRightTriple xs

-- Finds pair adding to 2020 and multiplies it together
find2020EntriesMultiplication :: ExpenseReport -> Int
find2020EntriesMultiplication = uncurry (*). findRightCombo . getPairCombos

-- Finds triple adding to 2020 and multiplies it together
find2020EntriesMultiplication' :: ExpenseReport -> Int
find2020EntriesMultiplication' = tripleOperation (*). findRightTriple . getTripleCombos

-- My puzzle input
main :: IO ()
main = do
  inputs <- lines <$> readFile "lib/day1.txt"
  let numbers = map read inputs :: [Int]
  putStrLn "part1:"
  print $ find2020EntriesMultiplication numbers
  putStrLn "part2:"
  print $ find2020EntriesMultiplication' numbers
