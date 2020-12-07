module Day1 (main, main') where

import Control.Applicative

-- Report Repair
type ExpenseReport = [Int]

-- ORIGINAL SOLUTION I CAME UP WITH
--

-- Creates all possible combinations of pairs from a list
getPairCombos :: [a] -> [(a, a)]
getPairCombos xs = liftA2 (,) xs xs

-- Creates all possible combinations of triples from a list
getTripleCombos :: [a] -> [(a, a, a)]
getTripleCombos xs = liftA3 (,,) xs xs xs

pairSumIs2020 :: (Int, Int) -> Bool
pairSumIs2020 = (==) 2020 . uncurry (+)

tripleOperation :: (Int -> Int -> Int) -> (Int, Int, Int) -> Int
tripleOperation f (a, b, c) = f (f a b) c

sumTriple :: (Int, Int, Int) -> Int
sumTriple = tripleOperation (+)

tripleSumIs2020 :: (Int, Int, Int) -> Bool
tripleSumIs2020 = (==) 2020 . sumTriple

findRightCombo :: [(Int, Int)] -> (Int, Int)
findRightCombo [] = error "Empty array passed to findRightCombo"
findRightCombo (x : xs) = if pairSumIs2020 x then x else findRightCombo xs

findRightTriple :: [(Int, Int, Int)] -> (Int, Int, Int)
findRightTriple [] = error "Empty array passed to findRightTriple"
findRightTriple (x : xs) = if tripleSumIs2020 x then x else findRightTriple xs

-- Finds pair adding to 2020 and multiplies it together
find2020EntriesMultiplication :: ExpenseReport -> Int
find2020EntriesMultiplication = uncurry (*) . findRightCombo . getPairCombos

-- Finds triple adding to 2020 and multiplies it together
find2020EntriesMultiplication' :: ExpenseReport -> Int
find2020EntriesMultiplication' = tripleOperation (*) . findRightTriple . getTripleCombos

main :: IO ()
main = do
  inputs <- lines <$> readFile "lib/day1.txt"
  let expenseReport = map read inputs :: [Int]
  putStrLn "part1:"
  print $ find2020EntriesMultiplication expenseReport
  putStrLn "part2:"
  print $ find2020EntriesMultiplication' expenseReport

-- NEW SOLUTION USING LIST COMPREHENSION WHICH I SAW ONLINE
--

main' :: IO ()
main' = do
  inputs <- lines <$> readFile "lib/day1.txt"
  let expenseReport = map read inputs :: [Int]
  putStrLn "part1:"
  print $ head [a * b | a <- expenseReport, b <- expenseReport, a + b == 2020]
  putStrLn "part2:"
  print $ head [a * b * c | a <- expenseReport, b <- expenseReport, c <- expenseReport, a + b + c == 2020]
