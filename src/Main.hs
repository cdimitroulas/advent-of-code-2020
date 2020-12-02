module Main where

import Day1
import Day2 as D2

main :: IO ()
main = do
  putStrLn "Day1 Part1 answer:"
  getReportRepairAnswer >>= print

  putStrLn "Day1 Part2 answer:"
  getReportRepairAnswer2 >>= print
  
  -- Day2:
  putStrLn "Day2 part 1 answer:"
  D2.findAnswer >>= print
  putStrLn "Day2 part 2 answer:"
  D2.findAnswer' >>= print
