module Main where

main = print . foldl1 lcm $ [1 .. 20]
