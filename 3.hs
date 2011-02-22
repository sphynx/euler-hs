module Main where

factors :: Integer -> [Integer]
factors x = factors' x 2
  where factors' x d
          | x == 1         = []
          | x `mod` d == 0 = d : factors' (x `div` d) d
          | otherwise      = factors' x (d + 1)

main :: IO ()
main = print . last . factors $ 600851475143
