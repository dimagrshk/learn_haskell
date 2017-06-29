lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number"
lucky x = "Not lucky"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String 
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You`re underweight"
    | bmi <= 25.0 = "You`re normal"
    | bmi <= 30.0 = "You`re fat"
    | otherwise                   = "You`re while"
    where bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = let sideArea = 2 * pi * r * h
                   topArea = pi * r ^ 2
               in sideArea + 2 * topArea

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False 
elem' a (x:xs) 
    | a == x = True
    | otherwise = a `elem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let
        smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in
        smallerSorted ++ [x] ++ biggerSorted

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] 
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  