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
doubleEveryOther = undefined

-- ex 3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- ex 4
validate :: Integer -> Bool
validate = undefined

-- ex 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

-- ex 6 (optional)
