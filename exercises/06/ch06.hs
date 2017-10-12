{-
Hutton 2e
Exercises: Chapter 6
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
{- Without a guard, a calling 'factorial' with a negative number never reaches
   the base case -}

fac :: Int -> Int
fac n
  | n <  0    = error "illegal argument"
  | n == 0    = 1
  | otherwise = n * fac (n - 1)


-- Ex. 2
sumdown :: Int -> Int
sumdown n
  | n <  0    = error "illegal argument"
  | n == 0    = 0
  | otherwise = n + sumdown (n - 1)


-- Ex. 3
(^^^) :: Int -> Int -> Int
_ ^^^ 0 = 1
x ^^^ n = x * (x ^^^ (n - 1))

{-
  2 ^ 3
= 2 * (2 ^ 2)
= 2 * 2 * (2 ^ 1)
= 2 * 2 * 2 * (2 ^ 0)
= 2 * 2 * 2 * 1
= 8
-}


-- Ex. 4
euclid :: Int -> Int -> Int
euclid x y =
  case compare x y of
    EQ -> x
    LT -> euclid x       (y - x)
    GT -> euclid (x - y) y


-- Ex. 5
{-
  length [1, 2, 3]
= 1 + length [2, 3]
= 1 + 1 + length [3]
= 1 + 1 + 1 + length []
= 1 + 1 + 1 + 0
= 3
-}

{-
  drop 3 [1,2,3,4,5]
= drop 2 [2,3,4,5]
= drop 1 [3,4,5]
= drop 0 [4,5]
= [4,5]
-}

{-
  init [1,2,3]
= 1:     $ init [2,3]
= 1:2:   $ init [3]
= 1:2:[]
= [1,2]
-}


-- Ex. 6
and' :: [Bool] -> Bool
and' []         = True
and' (False:_ ) = False
and' (_    :xs) = and' xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xxs) = xs ++ concat' xxs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x: replicate' (n - 1) x

nth :: [a] -> Int -> a
nth (x:_ ) 0 = x
nth (_:xs) n = nth xs (n-1)
nth []     _ = error "illegal argument"

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' y (x:xs)
  | x == y     = True
  | otherwise  = elem' y xs