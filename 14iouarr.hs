module Main where

import Data.List (maximumBy)
import Data.Word (Word64)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray, unsafeFreeze)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (assocs)
import Control.Monad (forM_)

type Cache = IOUArray Word64 Word64

size :: Word64
size = 1000000

collatz :: Cache -> Word64 -> IO Word64
collatz array n
  | n > size  = (+1) `fmap` collatz array (next n)
  | otherwise = do
      cached <- readArray array n
      case cached of
        0 -> do
          len <- collatz array (next n)
          let len' = len + 1
          writeArray array n len'
          return len'
        len ->
          return len

next :: Word64 -> Word64
next x | even x     = div x 2
       | otherwise  = 3 * x + 1

main :: IO ()
main = do
  arr <- newArray (1, size) 0
  writeArray arr 1 1
  forM_ [1 .. size] (collatz arr)
  frozen <- unsafeFreeze arr :: IO (UArray Word64 Word64)
  print . fst . maximumBy (\(_, v1) (_, v2) -> compare v1 v2) . assocs $ frozen
