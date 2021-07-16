-- Question 1
-- this factorial function is now not defined for negative numbers
fact' :: Int -> Int
fact' 0 = 1
fact' n | n >= 1 = n * fact' (n-1)

-- Question 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Question 3
exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' n e = n * exp' n (e - 1)

-- Question 4
euclid :: Int -> Int -> Int
euclid x y  | x > y = euclid y (x - y)
            | y > x = euclid x (y - x)
            | otherwise = x

-- Question 6
and' :: [Bool] -> Bool
and' [] = True
and' (False:_) = False
and' (True:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[]] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

-- errors if n is longer than length of list. Desired?
nthElem :: [a] -> Int -> a
nthElem (x:xs) 0 = x
nthElem (_:xs) n = nthElem xs (n-1)

elem' :: Eq a => [a] -> a -> Bool
elem' [] _               = False
elem' (x:xs) y  | x == y = True
                | otherwise = elem' xs y

-- Question 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys

-- Question 8
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs) where
        (as, bs) = halve xs

-- Question 9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)  | n == 0 = []
                | n > 0 = x : take (n-1) xs

last' :: [a] -> a -- question specifically says list is non empty
last' (x:[]) = x
last' (x:xs) = last' xs
