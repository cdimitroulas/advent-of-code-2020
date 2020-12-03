module Day3 where

type Coordinate = (Int, Int)

-- Access an x,y coordinate in a grid
accessCoordinate :: [[a]] -> Coordinate -> a
accessCoordinate grid (x, y) = (grid !! y) !! x

isTree :: Char -> Bool
isTree x = x == '#'

-- Given a grid and a list of coordinates, returns relevant the points
getPoints :: [[Char]] -> [Coordinate] -> [Char]
getPoints grid = filter isTree . map (accessCoordinate grid)

main :: IO ()
main = do
  -- Read the file and repeat each line infinitely
  contents <- map cycle . lines <$> readFile "lib/day3.txt"
  -- Create list of coordinates which we want to path through
  -- e.g. [(0, 0), (3, 1), (6, 2) ...]
  let path = zip [0, 3..] [0, 1.. length contents - 1]
  let answer = length $ getPoints contents path

  putStrLn "Day 3 part 1:"
  print answer

  putStrLn "Day 3 part 2:"
  -- Create a list of different paths
  let paths = [
          zip [0, 1..] [0, 1.. length contents - 1],
          path,
          zip [0, 5..] [0, 1.. length contents -1],
          zip [0, 7..] [0, 1.. length contents -1],
          zip [0, 1..] [0, 2.. length contents -1]
        ]
  -- Calculate the number of trees hit for each path and use `product`
  -- to multiply all the numbers together
  print $ product $ map (length . getPoints contents) paths
  
