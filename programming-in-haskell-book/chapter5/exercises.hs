-- Question 1

sumOfSquares = sum [x^2 | x <- [1..100]]

-- Question 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Question 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Question 4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-- Question 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) |
            x <- [1..n],
            y <- [x..n],
            z <- [y..n],
            x^2 + y^2 == z^2
          ]
-- Note: The question shows duplicates (3,4,5), (4,3,5)
-- but I think that this solution is more elegant because
-- there are no dupes. Although strictly speaking they're
-- different triples. As asked:
pyths' :: Int -> [(Int, Int, Int)]
pyths' n = [(x, y, z) |
            x <- [1..n],
            y <- [1..n],
            z <- [1..n],
            x^2 + y^2 == z^2
          ]

-- Question 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum . init . factors) x == x]

-- Question 7
question7TwoGenerators = [(x, y) | x <- [1,2], y <- [3,4]]
question7SingleGenerators = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]
question7IsSame = question7TwoGenerators == question7SingleGenerators

-- Question 8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [n | (x', n) <- zip xs [0..], x' == x]

-- Question 9
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]