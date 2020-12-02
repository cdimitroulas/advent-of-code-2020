module Main where

import qualified Day1 as D1
import qualified Day2 as D2

main :: IO ()
main = do
  D1.main
  
  -- Day2:
  putStrLn "Day2 part 1 answer:"
  D2.findAnswer >>= print
  putStrLn "Day2 part 2 answer:"
  D2.findAnswer' >>= print
