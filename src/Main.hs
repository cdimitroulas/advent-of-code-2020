module Main where

import Day1

main :: IO ()
main = do
  putStrLn "Day1 Part1 answer:"
  getReportRepairAnswer >>= print

  putStrLn "Day1 Part2 answer:"
  getReportRepairAnswer2 >>= print
