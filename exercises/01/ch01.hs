{-
Hutton 2e
Exercises: Chapter 1
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}


-- Ex. 1
{-
  double (double 2)
= double (2 + 2)       {applying inner 'double'}
= (2 + 2) + (2 + 2)    {applying 'double'}
= 4 + 4                {applying '+'}
= 8                    {applying '+'}
-}


-- Ex. 2
{-
Proof that sum [x] = x:
  sum [x]
= sum (x:[])    {de-sugaring [x]}
= x + sum []    {applying recursive case of 'sum'}
= x + 0         {applying base case of 'sum'}
= x             {applying '+'}
-}


-- Ex. 3
prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod xs

test_ex3 :: Bool
test_ex3 = prod ([2,3,4] :: [Integer]) == 24

{-
  prod [2,3,4]
= prod (2:[3,4])
= 2 * prod [3,4]
= 2 * prod (3:[4])
= 2 * 3 * prod [4]
= 2 * 3 * 4 * prod []
= 2 * 3 * 4
= 24
-}


-- Ex. 4
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where larger  = filter (>  x) xs
        smaller = filter (<= x) xs


-- Ex. 5
{- This modification of qsort would discard duplicates. -}