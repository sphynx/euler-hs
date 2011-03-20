module Main where

import Control.Monad.State.Strict
import qualified Data.Map as M

type MemoTable = M.Map Integer Integer

collatz :: Integer -> State MemoTable Integer
collatz n = do
  table <- get
  case M.lookup n table of
    Just len -> return len
    Nothing  -> do
      len <- collatz (next n)
      let len' = len + 1
      modify (M.insert n len')
      return len'

next :: Integer -> Integer
next x
  | even x     = div x 2
  | otherwise  = 3 * x + 1

main = do
  let table = M.singleton 1 1
  let filledTable = execState (mapM_ collatz [1 .. 1000000]) table
  -- find maximum (value, key) with fold:
  print . snd $ M.foldWithKey ((max .) . flip (,)) (0, 0) filledTable
