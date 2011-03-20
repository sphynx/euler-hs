module Main where

main = print $ f2n `div` (fn ^ 2)
  where fact = product . enumFromTo 1
        n    = 20
        fn   = fact n
        f2n  = fact (2 * n)
