module Main where

import Data.Word
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M

type KollatzNum = Word64
type MemoTable = M.HashMap KollatzNum KollatzNum

collatz :: KollatzNum -> State MemoTable KollatzNum
collatz n = do
  table <- get
  case M.lookup n table of
    Just len -> return len
    Nothing  -> do
      len <- collatz (next n)
      let len' = len + 1
      modify (M.insert n len')
      return len'

next :: KollatzNum -> KollatzNum
next x
  | even x     = div x 2
  | otherwise  = 3 * x + 1

main :: IO ()
main = do
  let table = M.singleton 1 1
  let filledTable = execState (mapM_ collatz [1 .. 10^6]) table
  print . snd $ M.foldlWithKey' (\a k v -> max a (v, k)) (0, 0) filledTable
