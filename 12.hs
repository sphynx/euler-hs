module Main where

factors :: Integer -> [Integer]
factors x = foldr (\d acc -> if x `mod` d == 0 then d : (x `div` d) : acc else acc) [] .
            takeWhile ((x >=) . (^2)) $ [1..]

triangles :: [Integer]
triangles = 1 : zipWith (+) triangles [2..]

main = print . head . dropWhile ((< 500) . length . factors) $ triangles
