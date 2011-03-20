module Main where

import Control.Monad.State.Strict

import Data.List
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
  let t = execState (mapM_ collatz [1 .. 1000000]) (M.singleton 1 1)
  print . fst $ M.foldWithKey f (0, 0) t
    where f k v (kmax, vmax)
            | v > vmax  = (k, v)
            | otherwise = (kmax, vmax)
