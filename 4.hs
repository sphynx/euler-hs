module Main where

isPalindrom :: (Show a) => a -> Bool
isPalindrom xs = (show xs) == reverse (show xs)

main :: IO ()
main = print $ maximum [z | x <- [100 .. 999], y <- [100 .. 999], let z = x * y, isPalindrom z]
