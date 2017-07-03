{----------- Exercise 1 --------------------------------}

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)


toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0      = []
    | otherwise   = mod x 10 : toDigitsRev (div x 10)


{----------- Exercise 2 --------------------------------}

{-
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOther' (reverse x))

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []             = []
doubleEveryOther' (h1 : [])      = [h1]
doubleEveryOther' (h1 : h2 : t) = h1 : (h2*2) : (doubleEveryOther' t)
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs  = [ if (even y) then (x*2) else x 
    | (x,y) <- zip xs (reverse (take (length xs) [1..])) ]

