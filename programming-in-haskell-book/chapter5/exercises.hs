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