module Day7 where

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Color = String

data BagRule = EmptyBag Color | BagContaining Color [Color] deriving (Show, Eq)

getColor :: BagRule -> Color
getColor (EmptyBag c) = c
getColor (BagContaining c _) = c

-- Notes:

-- * All colors are two words e.g. light red

-- * First two words are always the colour of the bag that the rule describes

-- * all contained bags follow the keyword "contain" and are separated by commas

-- * an empty bag is denoted as containing "no other bags". Treat "other" as a special color?

-- * the number of bags contained is rather meaningless for part1 so we can ignore them

--

strToBagRule :: String -> BagRule
strToBagRule input = case head containedBagColors of
  "other" -> EmptyBag bagColor
  _ -> BagContaining bagColor containedBagColors
  where
    bagColor = (unwords . take 2 . words) input
    -- Drop first 4 words which are always of form "light red bags contain"
    -- Then chunk into groups of 4 words and take the middle two
    -- since each group of 4 is always of the form "4 vivid green bags,"
    containedBagColors = (map (unwords . init . tail) . chunksOf 4 . drop 4 . words) input

-- Creates a Map of all the bag rules keyed by their color
strToBagRules :: String -> Map Color BagRule
strToBagRules input = Map.fromList $ map (toKeyBagRuleTuple . strToBagRule) $ lines input
  where
    toKeyBagRuleTuple :: BagRule -> (Color, BagRule)
    toKeyBagRuleTuple rule = (getColor rule, rule)

canContainGoldBag :: Map Color BagRule -> BagRule -> Bool
canContainGoldBag _ (EmptyBag _) = False
canContainGoldBag bagRuleMap (BagContaining _ containedColors) =
  ("shiny gold" `elem` containedColors)
    || any (canContainGoldBag bagRuleMap) bagRules
  where
    bagRules :: [BagRule]
    bagRules = map (bagRuleMap Map.!) containedColors

getNumberOfBagThatCanContainGold :: String -> Int
getNumberOfBagThatCanContainGold input = (Map.size . Map.filter canContainGoldBag') bagRuleMap
  where
    bagRuleMap = strToBagRules input
    -- partially apply canContainGoldBag with the map of all the bag rules
    canContainGoldBag' = canContainGoldBag bagRuleMap

part1 :: IO ()
part1 = readFile "lib/day7.txt" >>= print . getNumberOfBagThatCanContainGold
