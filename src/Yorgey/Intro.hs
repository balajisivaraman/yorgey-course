module Yorgey.Intro where
-- Solutions to exercises in 01-intro-hw.pdf

-- Ex 1
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Ex 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [_] = []
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x : y*2 : doubleEveryOther(xs)

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

-- Ex 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Tower of Hanoi Solutions
-- Ex 5
type Peg = String
type Move = (Peg, Peg)
type Disc = Integer
hanoi :: Disc -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
