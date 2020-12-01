module Day1 (getReportRepairAnswer, getReportRepairAnswer2) where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
-- import Control.Exception
import Data.Aeson

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

-- Tests
-- testExpenseReport1 :: ExpenseReport
-- testExpenseReport1 = [
--     1721,
--     979,
--     366,
--     299,
--     675,
--     1456
--   ]

-- testResult1 = assert (result == 514579) result
--   where
--     result = find2020EntriesMultiplication testExpenseReport1

-- testExpenseReport2 :: ExpenseReport
-- testExpenseReport2 = [
--     2020,
--     0,
--     1,
--     40,
--     49,
--     2039
--   ]

-- testResult2 = assert (result == 0) result
--   where
--     result = find2020EntriesMultiplication testExpenseReport2

-- My puzzle input
jsonFile :: FilePath
jsonFile = "lib/day1-1.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getPuzzleInput :: IO (Maybe ExpenseReport)
getPuzzleInput = decode <$> getJSON

getReportRepairAnswer :: IO Int
getReportRepairAnswer = maybe (error "Error parsing json file") find2020EntriesMultiplication <$> getPuzzleInput

getReportRepairAnswer2 :: IO Int
getReportRepairAnswer2 = maybe (error "Error parsing json file") find2020EntriesMultiplication' <$> getPuzzleInput
