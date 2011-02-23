module Main where

isTriplet :: Int -> Int -> Int -> Bool
isTriplet a b c = a ^ 2 + b ^ 2 == c ^ 2

triplets :: [Int]
triplets = [ a * b * c | a <- [1 .. 999],
                         b <- [a + 1 .. 999],
                         let c = 1000 - a - b,
                         isTriplet a b c ]

main = print . head $ triplets
