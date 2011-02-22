module Common where

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

factors :: Integer -> [Integer]
factors x = factors' x 2
  where factors' x d
          | x == 1         = []
          | x `mod` d == 0 = d : factors' (x `div` d) d
          | otherwise      = factors' x (d + 1)

isPalindrom :: (Show a) => a -> Bool
isPalindrom xs = (show xs) == reverse (show xs)

