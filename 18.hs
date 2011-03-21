module Main where

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS
import System.Environment

parse :: BS.ByteString -> [[Int]]
parse = map (map (fst . fromJust . BS.readInt) . BS.split ' ') . BS.lines

solve :: [Int] -> [Int] -> [Int]
solve up low = zipWith (+) up (zipWith max low (tail low))

main = do
  args <- getArgs
  file <- BS.readFile (args !! 0)
  print . maximum . foldr1 solve . parse $ file

