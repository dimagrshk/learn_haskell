{-example of using: INPUT: validate 4149437850368422
                    OUTPUT: TRUE -}

toDigits ::Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]


toDigitsRev ::Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10):toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = foldr(\x acc -> if odd (length acc) then 2*x:acc else x:acc) [] xs

-- | count the sum of digit
sumOfNumber :: Integer -> Integer
sumOfNumber 0 = 0
sumOfNumber n = (n `mod` 10) + sumOfNumber (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumOfNumber x) + sumDigits xs

validate :: Integer -> Bool
validate n = if ((sumDigits(doubleEveryOther(toDigits n))) `mod` 10) == 0
    then True
    else False
