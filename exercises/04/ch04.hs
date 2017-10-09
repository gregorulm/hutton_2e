{-
Hutton 2e
Exercises: Chapter 4
Gregor Ulm
-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs)
  where mid = length xs `div` 2


-- Ex. 2
third, third', third'' :: [a] -> a

third             = head . tail . tail

third'            = (!! 2)

third'' (_:_:x:_) = x


-- Ex. 3
safeTail, safeTail', safeTail'' :: [a] -> [a]

safeTail xs = if null xs then [] else tail xs

safeTail' xs
  | null xs   = []
  | otherwise = tail xs

safeTail'' []     = []
safeTail'' (_:xs) = xs


-- Ex. 4
{-
(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

False || False = False
_     || _     = True

False || b = b
True  || _ = True

b || c
  | b /= c    = True
  | otherwise = False
-}


-- Ex. 5
foo :: Bool -> Bool -> Bool
foo a b = if a then (if b then True else False) else False


-- Ex. 6
bar :: Bool -> Bool -> Bool
bar a b = if a then b else False


-- Ex. 7
mult :: Int -> Int -> Int -> Int
mult x = \y -> (\z -> x * y * z)


-- Ex. 8
luhnDouble :: Int -> Int
luhnDouble x = if tmp > 9 then tmp - 9 else tmp
  where tmp = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = 0 == (sum [luhnDouble a, b, luhnDouble c, d]) `mod` 10