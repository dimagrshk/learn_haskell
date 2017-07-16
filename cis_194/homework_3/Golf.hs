module Golf where

import Data.List
import Data.Function

--Exercise 1
skips :: [a] -> [[a]]
skips [] = []
skips all@(x:xs) = overlay 1 all

overlay :: Int -> [a] -> [[a]]
overlay n xs
    | n <= length xs = (testForSkips n xs):(overlay (n+1) xs)
    |otherwise = []

testForSkips :: Int -> [a] -> [a]
testForSkips n xs
    | n <= length xs = (xs !! (n - 1)):(testForSkips n (drop n xs))
    | otherwise = []

--Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (x:y:[]) = []
localMaxima all@(x:y:z:xs) = if ((x < y) && (z < y)) then y:localMaxima (drop 1 all) else localMaxima (drop 1 all)

--Exercise 3
histogram :: [Integer] -> String
histogram [] = []
histogram xs = output (prepList xs) ++ "=========\n" ++ "123456789\n"

forLine :: Integer -> [Integer] -> String
forLine 10 _ = []
forLine n xs = if n `elem` xs then '*':(forLine (n+1) xs) else ' ':(forLine (n+1) xs)

prepList :: [Integer] -> [[Integer]]
prepList xs = repeat' (unlock (groupBy ((==) `on` length) (reverse(sortBy (compare `on` length) (group (sort xs))))))

repeat' :: [[Integer]] -> [[Integer]]
repeat' [] = []
repeat' (x:[]) = [x]
repeat' (x:xs) = scanl (++) x (xs) 

unlock :: [[[Integer]]] -> [[Integer]]
unlock [] = []
unlock (x:xs) = (concat x):(unlock xs)

output :: [[Integer]] -> String
output [] = []
output (x:xs) = (forLine 1 x) ++ "\n" ++ (output xs)