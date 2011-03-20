-- thanks to http://en.wikipedia.org/wiki/English_numerals

module Main where

str :: Int -> String
str 1 = "one"
str 2 = "two"
str 3 = "three"
str 4 = "four"
str 5 = "five"
str 6 = "six"
str 7 = "seven"
str 8 = "eight"
str 9 = "nine"
str 10 = "ten"
str 11 = "eleven"
str 12 = "twelve"
str 13 = "thirteen"
str 14 = "fourteen"
str 15 = "fifteen"
str 16 = "sixteen"
str 17 = "seventeen"
str 18 = "eighteen"
str 19 = "nineteen"
str 20 = "twenty"
str 30 = "thirty"
str 40 = "forty"
str 50 = "fifty"
str 60 = "sixty"
str 70 = "seventy"
str 80 = "eighty"
str 90 = "ninety"
str 1000 = "onethousand"
str n
   | n > 20 && n < 100            = str (10 * (n `div` 10)) ++ str (n `mod` 10)
   | n >= 100 && n `mod` 100 == 0 = str (n `div` 100) ++ "hundred"
   | n > 100 && n < 1000          = str (n `div` 100) ++ "hundredand" ++ str (n `mod` 100)

main = print . length . concatMap str $ [1..1000]
