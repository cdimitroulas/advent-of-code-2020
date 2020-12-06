module Day2 (findAnswer, findAnswer') where

import Data.List.Split
import Text.Read

inputFilePath :: FilePath
inputFilePath = "lib/day2.txt"

-- Part 1
type Min = Int
type Max = Int
type Requirement = (Min, Max, Char)
type Password = [Char]

type PasswordEntry = (Requirement, Password)

parseRequirement :: String -> Maybe Requirement
parseRequirement str = do
  lowerBound <- readMaybe minStr :: Maybe Int
  upperBound <- readMaybe maxStr :: Maybe Int
  let pwChar = head char
  return (lowerBound, upperBound, pwChar)
  
  where
    req:(char:_) = splitOn " " str
    minStr:(maxStr:_) = splitOn "-" req

parseLine :: String -> Maybe PasswordEntry
parseLine x = do
  req <- parseRequirement reqStr
  -- we drop the first char as it's a space
  let (_:pw) = pwStr
  return (req, pw)

  where
    reqStr:(pwStr:_) = splitOn ":" x

isValidPasswordEntry :: PasswordEntry -> Bool
isValidPasswordEntry ((lower, upper, pwChar), pw) = charCount >= lower && charCount <= upper
  where
    charCount = length $ filter (== pwChar) pw

getNoOfValidPwEntries :: (PasswordEntry -> Bool) -> String -> Maybe Int
getNoOfValidPwEntries isValid fileContents = do
  pwEntries <- traverse parseLine $ lines fileContents
  return $ length $ filter isValid pwEntries

findAnswer :: IO (Maybe Int)
findAnswer = getNoOfValidPwEntries isValidPasswordEntry <$> readFile inputFilePath

-- Part 2
safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
        | (i > -1) && (length xs > i) = Just (xs !! i)
        | otherwise = Nothing

isValidPasswordEntry' :: PasswordEntry -> Bool
isValidPasswordEntry' ((pos1, pos2, pwChar), pw) = (charInPos1 && not charInPos2) || (not charInPos1 && charInPos2) 
  where
    charInPos1 = case safeIndex (pos1 - 1) pw of
                   Just char -> char == pwChar
                   Nothing -> False
    charInPos2 = case safeIndex (pos2 - 1) pw of
                   Just char -> char == pwChar
                   Nothing -> False

findAnswer' :: IO (Maybe Int)
findAnswer' = getNoOfValidPwEntries isValidPasswordEntry' <$> readFile inputFilePath
