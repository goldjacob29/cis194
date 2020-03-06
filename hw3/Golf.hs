module Golf where

import Data.List (transpose)


-- ex 1

-- return every nth starting at 1
skipEvery :: Int -> [a] -> [a]
skipEvery _ []     = []
skipEvery n (x:xs) = x : getEvery n xs

-- return every nth starting at n
getEvery :: Int -> [a] -> [a]
getEvery n xs = skipEvery n (drop n xs)

skips :: [a] -> [[a]]
skips xs = [getEvery i xs | i <- [0..n]]
    where n = length xs - 1

-- ex 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = 
    if b > a && b > c then 
        b : localMaxima (b:c:xs) 
    else localMaxima (b:c:xs)
localMaxima _ = []

-- ex 3
getFrequency :: Integer -> [Integer] -> Int
getFrequency x = length . filter (==x)

makeBar :: Int -> Int -> String
makeBar f h = 
    let
        stars = replicate f '*'
    in
        take h $ stars ++ repeat ' '


-- histogram :: [Integer] -> String
histogram xs = unlines makeHistogram
    where
        frequencies = [getFrequency i xs | i <- [0..9]]
        barHeight = maximum frequencies
        bars = [makeBar f barHeight | f <- frequencies]
        makeHistogram = (reverse . transpose) bars ++ ["==========", "0123456789"]

