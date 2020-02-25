module Hw1 where

-- ex 1
toDigits :: Integer -> [Integer]
toDigits x 
  | x <= 0 = []
  | otherwise = (toDigits $ x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : (toDigitsRev $ x `div` 10)

-- ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = 
    let
        double _ [] = []
        double True  (x:xs) = (x*2) : double False xs
        double False (x:xs) =     x : double True xs
    in
        reverse $ double False $ reverse xs

-- ex 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits $ toDigits x ++ xs

-- ex 4
validate :: Integer -> Bool
validate = (verify . sumDigits . doubleEveryOther . toDigits)
    where verify x = (x `mod` 10) == 0

-- ex 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

-- ex 6 (optional)
