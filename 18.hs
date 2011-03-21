module Main where

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS
import System.Environment

parse :: BS.ByteString -> [[Int]]
parse = map (map (fst . fromJust . BS.readInt) . BS.split ' ') . BS.lines

solve :: [Int] -> [Int] -> [Int]
solve [] [x] = [x]
solve up low =
  let first  =     zipWith (+) up low        ++ [0]
      second = 0 : zipWith (+) up (tail low)
  in zipWith max first second

main = do
  args <- getArgs
  file <- BS.readFile (args !! 0)
  print . maximum . foldl' solve [] . parse $ file

