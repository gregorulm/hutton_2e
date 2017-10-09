{-
Hutton 2e
Exercises: Chapter 5
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
square100 :: Int
square100 = sum $ [ x * x | x <- [1..100] ]


-- Ex. 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n] ]


-- Ex. 3
square :: Int -> [(Int, Int)]
square a = [ (x, y) | (x, y) <- grid a a, x /= y ]


-- Ex. 4
repl' :: Int -> a -> [a]
repl' n val = [ val | _ <- [1..n] ]


-- Ex. 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n],
                        y <- [1..n],
                        z <- [1..n],
                        x * x + y * y == z * z ]


-- Ex. 6
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],
                  (sum $ factors x) - x == x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


-- Ex. 7
-- [(x,y) | x <- [1,2], y <- [3,4]] == [(1,3),(1,4),(2,3),(2,4)]
nested :: [(Int, Int)]
nested = concat [ [(a,  b)  | b <- bar ] | a <- foo ]
  where foo = [x | x <- [1,2]]
        bar = [y | y <- [3,4]]


-- Ex. 8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [1..]
--positions x xs = find x [ (a, b) | (a, b) <- zip xs [1..] ]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']


-- Ex. 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum $ [ x * y | (x, y) <- zip xs ys ]


-- Ex. 10
-- cf. cipher.hs
