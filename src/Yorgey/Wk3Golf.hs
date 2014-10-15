module Yorgey.Wk3Golf where

import Test.QuickCheck

-- The tests in below comments can be executed using Doctest.
-- I still haven't automated the testing though. That's the next step.

-- | The output of skips is a list of lists. The list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips []
-- []
--
-- prop> length (skips s) == length s
skips :: [a] -> [[a]]
skips l = map getItems [1..length l]
  where
    getItems n = map (l !!) [n-1,2*n-1..length l - 1]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
  | x < y && z < y = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
