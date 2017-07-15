module Golf where

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