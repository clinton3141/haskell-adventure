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

-- concat' :: [[a]] -> [a]
-- concat' [] = []
-- concat' [[]:xss] = xss
-- concat' [xs:xss] =  xs ++ concat' xss
