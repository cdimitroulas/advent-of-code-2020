module Day2 (findAnswer, findAnswer') where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

inputFilePath :: FilePath
inputFilePath = "lib/day2.txt"

readExerciseData :: IO B.ByteString
readExerciseData = B.readFile inputFilePath

-- Part 1
type Min = Int
type Max = Int
type Requirement = (Min, Max, Char)
type Password = [Char]

type PasswordEntry = (Requirement, Password)

parseRequirement :: B.ByteString -> Maybe Requirement
parseRequirement str = do
  (min, _) <- BC.readInt minStr
  (max, _) <- BC.readInt maxStr
  let pwChar = head $ BC.unpack char
  return (min, max, pwChar)
  
  where
    req:(char:_) = BC.split ' ' str
    minStr:(maxStr:_) = BC.split '-' req

parseLine :: B.ByteString -> Maybe PasswordEntry
parseLine x = do
  req <- parseRequirement reqStr
  -- we drop the first char as it's a space
  let (_:pw) = BC.unpack pwStr
  return (req, pw)

  where
    reqStr:(pwStr:_) = BC.split ':' x

isValidPasswordEntry :: PasswordEntry -> Bool
isValidPasswordEntry ((min, max, pwChar), pw) = charCount >= min && charCount <= max
  where
    charCount = length $ filter (== pwChar) pw

getNoOfValidPwEntries :: (PasswordEntry -> Bool) -> B.ByteString -> Maybe Int
getNoOfValidPwEntries isValid fileContents = do
  pwEntries <- traverse parseLine $ BC.lines fileContents
  return $ length $ filter isValid pwEntries

findAnswer :: IO (Maybe Int)
findAnswer = getNoOfValidPwEntries isValidPasswordEntry <$> readExerciseData

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
findAnswer' = getNoOfValidPwEntries isValidPasswordEntry' <$> readExerciseData
