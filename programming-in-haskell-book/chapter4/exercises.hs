-- Question 1

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Question 2
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then []
                else tail xs

safetail2 :: [a] -> [a]
safetail2 xs  | null xs = []
              | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs


-- Question 3
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