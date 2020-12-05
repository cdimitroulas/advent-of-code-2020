module Day4 where

import qualified Data.List as List
import Data.List.Split
import qualified Data.Map as Map

-- Utility to split a string on a particular character into a tuple
-- e.g. splitOnFirst ':' "key:value" -> ("key", "value")
splitOnFirst :: Char -> String -> Maybe (String, String)
splitOnFirst char str = (\i -> (List.take i str, drop (i+1) str)) <$> List.elemIndex char str

parseKeysAndVals :: [String] -> Maybe (Map.Map String String)
-- We remove the "cid" key as it's not relevant for our logic
parseKeysAndVals str = Map.delete "cid" . Map.fromList <$> (traverse $ splitOnFirst ':') str

justs :: [Maybe a] -> [a]
justs [] = []
justs (x:xs) = case x of
                 Just y -> y : justs xs
                 Nothing -> justs xs

main :: IO ()
main = do
  contents <- splitOn "\n\n" <$> readFile "lib/day4.txt"
  let passports = (justs . map (parseKeysAndVals . filter (/= "") . splitOneOf " \n")) contents

  putStrLn "Day 4 part 1:"
  -- We count how many passports have all 7 fields
  print $ length $ (filter ((== 7). Map.size) . passports
