{-
Hutton 2e
Exercises: Chapter 3
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
exp1 :: String
exp1 = ['a', 'b', 'c']

exp2 :: (Char, Char, Char)
exp2 = ('a', 'b', 'c')

exp3 ::[(Bool, Char)]
exp3 = [(False, 'O'), (True, '1')]

exp4 :: ([Bool], [Char])
exp4 = ([False, True], ['0', '1'])

exp5 :: [[a] -> [a]]
exp5 = [tail, init, reverse]


-- Ex. 2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2,3], [2,4,6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = (f x)


-- Ex. 3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Ord a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- Ex. 5
{-
It is generally not feasible for Haskell functions to be instances of the class
Eq because the type system cannot express that two functions of the same type
produce the same output for every possible input.
-}