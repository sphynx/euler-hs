module Main where

import Data.Maybe
import qualified Data.ByteString.Char8 as BS

parse :: BS.ByteString -> [Integer]
parse = map (fst . fromJust . BS.readInteger) . BS.lines

main = do
  file <- BS.readFile "13.txt"
  putStrLn . take 10 . show . sum . parse $ file
