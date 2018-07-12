Validate credit card

> toDigits :: Integer -> [Integer]
> toDigits 0 = []
> toDigits x = toDigits( x `div` 10) ++ [x `mod` 10]

> toDigitsRev :: Integer -> [Integer]
> toDigitsRev 0 = []
> toDigitsRev x = [x `mod` 10] ++ toDigitsRev(x `div` 10)

> doubleEveryOther :: [Integer] -> [Integer]
> doubleEveryOther [] = []
> doubleEveryOther (x:[]) = [x]
> doubleEveryOther (x:y:zs)
>  | (length (x:y:zs)) `mod` 2 /= 0 = x : y*2 : doubleEveryOther zs
>  | otherwise = x*2 : y : doubleEveryOther zs

> sumDigits :: [Integer] -> Integer
> sumDigits [] = 0
> sumDigits (x:[])
>   | x < 10 = x
>   | otherwise = sumDigits(toDigitsRev x)
> sumDigits (x:zs) = sumDigits(toDigitsRev x) + sumDigits(zs)

> validate :: Integer -> Bool
> validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0

The Towers of Hanoi

> type Peg = String
> type Move = (Peg, Peg)
> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

> hanoi 0 _ _ _ = []
> hanoi 1 a b c = [(a, c)]
> hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,c)] ++ (hanoi (n-1) b a c)
