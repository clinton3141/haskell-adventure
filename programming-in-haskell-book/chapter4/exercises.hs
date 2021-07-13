-- Question 1 (first edition)

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Question 2 (first edition)
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then []
                else tail xs

safetail2 :: [a] -> [a]
safetail2 xs  | null xs = []
              | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs


-- Question 3 (first edition)
or1 :: Bool -> Bool -> Bool
or1 True True = True
or1 True False = True
or1 False True = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _ _ = False

or3 :: Bool -> Bool -> Bool
or3 False False = False
or3 _ _ = True

or4 :: Bool -> Bool -> Bool
or4 True True = True
or4 False a = a
or4 a False = a


-- Question 4 (first edition)
-- True ^ True = True; _ ^ _ = False
and' :: Bool -> Bool -> Bool
and' a b  = if a then if b then True
            else False else False

-- Question 5 (first edition)
-- True ^ b = b; False ^ _ = False
and'' :: Bool -> Bool -> Bool
and'' a b = if a then b else False

-- Question 6 (first edition)
-- mult x y z = x * y * z
mult' :: Num a => a -> a-> a -> a
mult' = \x -> (\y -> (\z -> x * y * z))


-- Question 8 (second edition)
luhnDouble :: Int -> Int
luhnDouble n  | n < 5 = n * 2
              | otherwise = n * 2 - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
-- TODO: looking forward to being able to do that in a more Haskell way