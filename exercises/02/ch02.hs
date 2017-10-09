{-
Hutton 2e
Exercises: Chapter 2
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 2
{-
(2^3)*4
(2+3)+(4*5)
2+(3*4)^5
-}


-- Ex. 3
n :: Int
n = a `div` length xs
  where
  a  = 10
  xs = [1,2,3,4,5] :: [Integer]


-- Ex. 4
last' :: [a] -> a
last' xs = head $ drop (length xs - 1) xs

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)
-- just like with Prelude.last, an empty list leads to a runtime error


-- Ex. 5
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

init'' :: [a] -> [a]
init'' = reverse . tail. reverse