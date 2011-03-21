module Main where

import Data.List

type Day = Int -- 0 to 6
type Year = Int
type Leapness = Bool

s1perYear :: Day -> Bool -> Int
s1perYear 0 False = 2
s1perYear 1 False = 2
s1perYear 2 False = 1
s1perYear 3 False = 3
s1perYear 4 False = 1
s1perYear 5 False = 1
s1perYear 6 False = 2
s1perYear 0 True  = 2
s1perYear 1 True  = 1
s1perYear 2 True  = 2
s1perYear 3 True  = 2
s1perYear 4 True  = 1
s1perYear 5 True  = 1
s1perYear 6 True  = 3

nextYearStart :: Day -> Bool -> Day
nextYearStart d True  = (d + 2) `mod` 7
nextYearStart d False = (d + 1) `mod` 7

leap :: Year -> Leapness
leap n = n `mod` 4 == 0 && not (n `mod` 400 == 0)

main = print . fst . foldl' f (0, 1) $ [1901 .. 2000]
  where
    f (n, start) y = let l = leap y in (n + s1perYear start l, nextYearStart start l)
