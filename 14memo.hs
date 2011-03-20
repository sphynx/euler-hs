module Main where

import Data.List
import qualified Data.Map as M

type MemoTable = M.Map Integer Integer

memoCollatz :: MemoTable -> Integer -> (Integer, MemoTable)
memoCollatz table n = case M.lookup n table of
  Just len -> (len, table)
  Nothing  -> let (len, table') = memoCollatz table (next n)
              in  (len + 1, M.insert n (len + 1) table')

next :: Integer -> Integer
next x
  | even x     = div x 2
  | otherwise  = 3 * x + 1

memoTable :: Integer -> MemoTable
memoTable = foldl' f (M.singleton 1 1) . enumFromTo 1
  where f acc x = snd $ memoCollatz acc x

size = 1000000

main = print $ M.foldWithKey f (0, 0) (memoTable size)
  where f k v (kmax, vmax)
          | v > vmax  = (k, v)
          | otherwise = (kmax, vmax)
