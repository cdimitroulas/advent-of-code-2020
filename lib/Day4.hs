module Day4 where

import qualified Data.List as List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Text.Read
import Text.Regex

-- Part 1

-- Utility to split a string on a particular character into a tuple
-- e.g. splitOnFirst ':' "key:value" -> ("key", "value")
splitOnFirst :: Char -> String -> Maybe (String, String)
splitOnFirst char str = (\i -> (List.take i str, drop (i+1) str)) <$> List.elemIndex char str

parseKeysAndVals :: [String] -> Maybe (Map String String)
-- We remove the "cid" key as it's not relevant for our logic
parseKeysAndVals str = Map.delete "cid" . Map.fromList <$> (traverse $ splitOnFirst ':') str

justs :: [Maybe a] -> [a]
justs [] = []
justs (x:xs) = case x of
                 Just y -> y : justs xs
                 Nothing -> justs xs

part1 :: IO ()
part1 = do
  contents <- splitOn "\n\n" <$> readFile "lib/day4.txt"
  let passports = (justs . map (parseKeysAndVals . filter (/= "") . splitOneOf " \n")) contents

  putStrLn "Day 4 part 1:"
  -- We count how many passports have all 7 fields
  print $ length $ filter ((== 7). Map.size) passports

-- Part 2

newtype BirthYear = BirthYear Int deriving (Show)

mkByr :: String -> Maybe BirthYear
mkByr str = (readMaybe str :: Maybe Int) >>= \num ->
  if num >= 1920 && num <= 2020 then Just $ BirthYear num else Nothing

newtype IssueYear = IssueYear Int deriving (Show)

mkIyr :: String -> Maybe IssueYear
mkIyr str = (readMaybe str :: Maybe Int) >>= \num -> 
  if num >= 2010 && num <= 2020 then Just $ IssueYear num else Nothing

newtype ExpYear = ExpYear Int deriving (Show)

mkEyr :: String -> Maybe ExpYear
mkEyr str = (readMaybe str :: Maybe Int) >>= \num ->
  if num >= 2020 && num <= 2030 then Just $ ExpYear num else Nothing

newtype Height = Height (Int,String) deriving (Show)

mkHgt :: String -> Maybe Height
mkHgt (a:b:"in") = (readMaybe [a, b] :: Maybe Int) >>= \num ->
  if  num >= 59 && num <= 76 then Just $ Height (num, "in") else  Nothing
mkHgt (a:b:c:"cm") = (readMaybe [a,b,c] :: Maybe Int) >>= \num ->
  if  num >= 150 && num <= 193 then Just $ Height (num, "cm") else  Nothing
mkHgt _ = Nothing

newtype HairColor = HairColor String deriving (Show)

mkHcl :: String -> Maybe HairColor
mkHcl [] = Nothing
mkHcl str@(x:xs) = if (x == '#') && length xs == 6 &&
  isJust (matchRegex (mkRegex "^[0-9a-f]{6}$") xs)
                      then Just $ HairColor str
                      else Nothing

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Show)

mkEcl :: String -> Maybe EyeColor
mkEcl str
  | str == "amb" = Just Amb
  | str == "blu" = Just Blu
  | str == "brn" = Just Brn
  | str == "gry" = Just Gry
  | str == "grn" = Just Grn
  | str == "hzl" = Just Hzl
  | str == "oth" = Just Oth
  | otherwise = Nothing

newtype PassportId = PassportId String deriving (Show)

mkPid :: String -> Maybe PassportId
mkPid str = if isJust (matchRegex (mkRegex "^[0-9]{9}$") str) then Just $ PassportId str else Nothing

data Passport = Passport {
  byr :: BirthYear,
  iyr :: IssueYear,
  eyr :: ExpYear,
  hgt :: Height,
  hcl :: HairColor,
  ecl :: EyeColor,
  pid :: PassportId
  }

mkPassport :: Map String String -> Maybe Passport
mkPassport fields = Passport <$> (Map.lookup "byr" fields >>= mkByr)
  <*> (Map.lookup "iyr" fields >>= mkIyr)
  <*> (Map.lookup "eyr" fields >>= mkEyr)
  <*> (Map.lookup "hgt" fields >>= mkHgt)
  <*> (Map.lookup "hcl" fields >>= mkHcl)
  <*> (Map.lookup "ecl" fields >>= mkEcl)
  <*> (Map.lookup "pid" fields >>= mkPid)

part2 :: IO ()
part2 = do
  contents <- splitOn "\n\n" <$> readFile "lib/day4.txt"
  let passportData = (justs . map (parseKeysAndVals . filter (/= "") . splitOneOf " \n")) contents

  putStrLn "Day 4 part 2:"
  print $ length $ (filter isJust . map mkPassport) passportData
