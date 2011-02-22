module Main where

-- Eratosphenes sieve

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

-- 51 seconds
-- main = print (primes !! 10000)

-- 1.4 seconds
main = print $ (primesUpTo 150000) !! 10000
