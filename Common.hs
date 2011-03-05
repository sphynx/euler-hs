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

-- slow, but gives a nice infinite list
primes :: [Integer]
primes = primes' [2 ..]
  where
    f k = filter ((/= 0) . (`mod` k))
    primes' (x:xs) = x : primes' ((f x) xs)

-- faster but needs an upper limit
primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2 .. n]
  where
    f k = filter ((/= 0) . (`mod` k))
    sieve (x:xs)
          | x * x < n  = x : sieve ((f x) xs)
          | otherwise  = x : xs
