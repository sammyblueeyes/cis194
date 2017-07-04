{----------- Exercise 1 --------------------------------}

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)


toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0      = []
    | otherwise   = mod x 10 : toDigitsRev (div x 10)


{----------- Exercise 2 --------------------------------}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOther' (reverse x) []

doubleEveryOther' :: [Integer] -> [Integer] -> [Integer]
doubleEveryOther' []  l           = l
doubleEveryOther' (h1 : [])  l    = h1 : l
doubleEveryOther' (h1 : h2 : t) l = doubleEveryOther' t  ((h2*2) : h1 : l)

{-
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs  = [ if (even y) then (x*2) else x 
    | (x,y) <- zip xs (reverse (take (length xs) [1..])) ]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs  = reverse [ if (even y) then (x*2) else x 
    | (x,y) <-  (zip (reverse xs) [1..]) ]
-}

{----------- Exercise 3 --------------------------------}

sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (h : t) = sum (toDigits h) + sumDigits t

{----------- Exercise 4 --------------------------------}

validate :: Integer -> Bool
validate n = (sumDigits (reverse (doubleEveryOther' (toDigitsRev n) [])) `mod` 10) == 0

{----------- Exercise 5 --------------------------------}

