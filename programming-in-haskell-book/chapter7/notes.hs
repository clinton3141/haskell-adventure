-- fold right encapsulates base case plus f (x:xs) = x # f xs -- where # is an operator such as +
-- e.g. sum' [] = 0; sum' (x:xs) = x + sum xs
-- becomes sum' xs = foldr (+) 0 xs
-- or better: sum' = foldr (+) 0
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- can be useful to think of it "replacing" cons with # and [] with base value
-- e.g. sum (1:(2:[])) == (1 + (2 + (0)))

length' :: [a] -> Int
length' = foldr (\_ n -> n + 1) 0

-- fold left does the same but for operations which associate to the left
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

-- as a "replacement" sum' [1,2] = (((0) + 1) + 2)

length'' :: [a] -> Int
length'' = foldl' (\n _ -> n + 1) 0
