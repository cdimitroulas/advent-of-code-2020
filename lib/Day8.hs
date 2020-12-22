module Day8 where

import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Read

mkSign :: Char -> Maybe Int
mkSign '+' = Just 1
mkSign '-' = Just (-1)
mkSign _ = Nothing

-- Data type for the different possible operations that the program can have
data Command = Accum | Jump | Noop deriving (Eq, Show)

type Instruction = (Command, Int)

mkInstruction :: String -> Maybe Instruction
mkInstruction str = do
  let (commandNameStr : numStr : _) = words str
  sign <- mkSign $ head numStr
  num <- readMaybe (tail numStr) :: Maybe Int
  case commandNameStr of
    "acc" -> Just (Accum, sign * num)
    "jmp" -> Just (Jump, sign * num)
    "nop" -> Just (Noop, sign * num)
    _ -> Nothing

isNoop :: Instruction -> Bool
isNoop (Noop, _) = True
isNoop _ = False

isJump :: Instruction -> Bool
isJump (Jump, _) = True
isJump _ = False

-- Stateful things to track:
--  - The accumulator value
--  - The line number to execute next
--  - Which lines have already been executed
data ExecutionState = ExecutionState
  { progCommands :: [Instruction],
    progNextLineNo :: Int,
    progAccum :: Int,
    progExecutedLineNos :: [Int]
  }
  deriving (Show)

mkInitialState :: [Instruction] -> ExecutionState
mkInitialState cmds =
  ExecutionState
    { progCommands = cmds,
      progNextLineNo = 0,
      progAccum = 0,
      progExecutedLineNos = []
    }

getNextLineNo :: Int -> Instruction -> Int
getNextLineNo index (Jump, num) = index + num
getNextLineNo index _ = index + 1

getNextAccum :: Int -> Instruction -> Int
getNextAccum accum (Accum, num) = accum + num
getNextAccum accum _ = accum

-- If we reach a line which has already been executed, then we want to return the accumulator
-- value. This is done by returning a Maybe from the stateful effect which is Nothing unless
-- the next line to execute has already been executed previously.
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
  return
    ( case result of
        Just x -> x
        Nothing -> runProgram newExecState
    )

getProgramAccumValue :: String -> Maybe Int
getProgramAccumValue str = do
  cmds <- (mapM mkInstruction . lines) str
  return $ runProgram $ mkInitialState cmds

part1 :: IO ()
part1 = readFile "lib/day8.txt" >>= (print . getProgramAccumValue)

-- Part 2
-- ----------------------

-- We take our list of commands, we go through it and every time
-- we find a nop/jmp we add a new list of commands to an array, with this command switched.

switchJmpNoop :: Instruction -> Instruction
switchJmpNoop (Jump, num) = (Noop, num)
switchJmpNoop (Noop, num) = (Jump, num)
switchJmpNoop x = x

getExecutedProgVal :: Seq Instruction -> Int
getExecutedProgVal prg = head $ do
  i <- [0 ..]
  Right a <- return $ trace (show $ f $ Seq.adjust' switchJmpNoop i prg) f $ Seq.adjust' switchJmpNoop i prg
  return a

-- Returns a Left if there's an infinite loop, a Right otherwise
f :: Seq Instruction -> Either Int Int
f prg = exec 0 0 Set.empty
  where
    exec :: Int -> Int -> Set Int -> Either Int Int
    exec accum index s =
      if index `Set.member` s
        then Left accum
        else case index `Seq.lookup` prg of
          -- if the index is not in prg then it means we've terminated the program
          Nothing -> Right accum
          -- if it is a valid index, then we need to keep executing
          Just cmd -> exec (getNextAccum index cmd) (getNextLineNo index cmd) (Set.insert index s)

part2 :: IO ()
part2 = do
  cmdsM <- mapM mkInstruction . lines <$> readFile "lib/day8-test.txt"
  print $ getExecutedProgVal . Seq.fromList <$> cmdsM
