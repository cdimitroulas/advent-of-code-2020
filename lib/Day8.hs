module Day8 where

import Control.Monad.State
import Debug.Trace
import Text.Read

newtype Direction = Direction Char deriving (Eq, Show)

mkDirection :: Char -> Maybe Direction
mkDirection '+' = Just $ Direction '+'
mkDirection '-' = Just $ Direction '-'
mkDirection _ = Nothing

getDirectionOperator :: Num a => Direction -> (a -> a -> a)
getDirectionOperator dir = if unDirection dir == '+' then (+) else (-)

unDirection :: Direction -> Char
unDirection (Direction char) = char

data Command = Accum Direction Int | Jump Direction Int | Noop deriving (Eq, Show)

mkCommand :: String -> Maybe Command
mkCommand str = do
  let (commandNameStr : numStr : _) = words str
  dir <- mkDirection $ head numStr
  num <- readMaybe (tail numStr) :: Maybe Int
  case commandNameStr of
    "acc" -> Just $ Accum dir num
    "jmp" -> Just $ Jump dir num
    "nop" -> Just Noop
    _ -> Nothing

data ExecutionState = ExecutionState
  { progCommands :: [Command],
    progNextLineNo :: Int,
    progAccum :: Int,
    progExecutedLineNos :: [Int]
  }
  deriving (Show)

mkInitialState :: [Command] -> ExecutionState
mkInitialState cmds = ExecutionState cmds 0 0 []

getNextLineNo :: Int -> Command -> Int
getNextLineNo index (Jump dir amt) = getDirectionOperator dir index amt
getNextLineNo index _ = index + 1

getNextAccum :: Int -> Command -> Int
getNextAccum accum (Accum dir amt) = getDirectionOperator dir accum amt
getNextAccum accum _ = accum

-- Stateful things to track:
--  - The accumulator value
--  - The line number to execute next
--  - Which lines have already been executed
--
-- If we reach a line which has already been executed, then we want to return the accumulator
-- value.

runNextLine :: State ExecutionState (Maybe Int)
runNextLine = state $ \s ->
  let nextCommand = progCommands s !! progNextLineNo s

      nextLineNo = getNextLineNo (progNextLineNo s) nextCommand

      newAccum = getNextAccum (progAccum s) nextCommand

      newState =
        ExecutionState
          { progCommands = progCommands s,
            progNextLineNo = nextLineNo,
            progAccum = newAccum,
            progExecutedLineNos = progExecutedLineNos s <> [nextLineNo]
          }

      value = if nextLineNo `elem` progExecutedLineNos s then Just $ progAccum s else Nothing
   in (value, newState)

runProgram :: ExecutionState -> Int
runProgram = do
  (result, newExecState) <- runState runNextLine
  case result of
    Just x -> return x
    Nothing -> return $ runProgram newExecState

getProgramAccumValue :: String -> Maybe Int
getProgramAccumValue str = do
  cmds <- (mapM mkCommand . lines) str
  return $ runProgram $ mkInitialState cmds

part1 :: IO ()
part1 = readFile "lib/day8.txt" >>= (print . getProgramAccumValue)
