module Day3 where

type Coordinate = (Int, Int)

-- Access an x,y coordinate in a grid
accessCoordinate :: [[a]] -> Coordinate -> a
accessCoordinate grid (x, y) = (grid !! y) !! x

isTree :: Char -> Bool
isTree x = x == '#'

type Right = Int

type Down = Int

type Height = Int

-- Given a number of steps right and a number of steps down + the height of the grid,
-- returns a list of coordinates for the path through the grid
-- e.g. createPath 3 1 = [(0, 0), (3, 1), (6, 2)...]
createPath :: Right -> Down -> Height -> [Coordinate]
createPath right down h = zip [0, right ..] [0, down .. h]

-- Given a grid and a list of coordinates, returns relevant the points
getPoints :: [[Char]] -> [Coordinate] -> [Char]
getPoints grid = filter isTree . map (accessCoordinate grid)

main :: IO ()
main = do
  -- Read the file and repeat each line infinitely
  contents <- map cycle . lines <$> readFile "lib/day3.txt"
  let slopeHeight = length contents - 1
  let path = createPath 3 1 slopeHeight
  let answer = length $ getPoints contents path

  putStrLn "Day 3 part 1:"
  print answer

  putStrLn "Day 3 part 2:"
  -- Create a list of different paths
  let paths =
        [ createPath 1 1 slopeHeight,
          path,
          createPath 5 1 slopeHeight,
          createPath 7 1 slopeHeight,
          createPath 1 2 slopeHeight
        ]
  -- Calculate the number of trees hit for each path and use `product`
  -- to multiply all the numbers together
  print $ product $ map (length . getPoints contents) paths
