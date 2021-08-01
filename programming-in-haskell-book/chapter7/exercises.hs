-- Question 1
q1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
q1 f p xs = [f x | x <- xs, p x]

q1' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
q1' f p xs = map f (filter p xs)

-- Question 2
all' :: (a -> Bool) -> [a] -> Bool -- Question has (a -> Bool) -> [Bool] -> Bool, but presumably this is wrong
all' p []       = True
all' p (x:xs)   | not (p x) = False
                | otherwise = all' p xs

any' :: (a -> Bool) -> [a] -> Bool -- Similar to above
any' p []       = False
any' p (x:xs)   | p x       = True
                | otherwise = any' p xs


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) | p x       = x : takeWhile p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- Question 3
map':: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- Question 4
-- first attempt 🤢
-- dec2int :: [Int] -> Int
-- dec2int is = foldl (\r i -> r + fst i * snd i) 0 (zip (reverse is) (iterate(*10) 1))

-- second 😌
dec2int :: [Int] -> Int
dec2int = foldl (\r i -> r * 10 + i) 0

-- Question 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Question 6
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (==0) (`mod` 2) (`div` 2)

type Bit = Int
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

-- this is very verbose, but helped me to grok it
-- can be iterate'' = unfold (const False) id
iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = unfold neverEmpty id f x
            where neverEmpty = const False

-- Question 7, 8
-- See transmitter.hs

-- Question 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g xs = (f . head) xs : altMap g f (tail xs)

-- Question 10
luhnDouble :: Int -> Int
luhnDouble n  | n < 5     = n * 2
              | otherwise = n * 2 - 9

luhn :: [Int] -> Bool
luhn = (\x -> x `mod` 10 == 0) . sum . altMap luhnDouble id
