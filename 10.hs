module Main where

import Common (primesUpTo)

main :: IO ()
main = print . sum . primesUpTo $ 2000000
