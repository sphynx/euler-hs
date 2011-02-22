module Main where

sumOfSquares :: Integer -> Integer
sumOfSquares n = sum . map (^2) $ [1 .. n]

squareOfSum :: Integer -> Integer
squareOfSum n = (^2) . sum $ [1 .. n]

main :: IO ()
main = print (squareOfSum 100 - sumOfSquares 100)
