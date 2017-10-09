{-
Hutton 2e
Exercises: Chapter 5
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
square100 :: Int
square100 = sum $ [ x * x | x <- [1..100]]


-- Ex. 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]] 


-- Ex. 3
square :: Int -> [(Int, Int)]
square a = [(x, y) | (x, y) <- grid a a, x /= y]

-- Ex. 4
repl' :: Int -> a -> [a]
repl' n val = [x | x <- [1..n]]

