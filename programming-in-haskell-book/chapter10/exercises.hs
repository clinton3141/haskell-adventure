-- question 1
putStr' :: String -> IO ()
putStr' ss = sequence_ [putChar c | c <- ss]

-- question 2, 3 not included here since they rely on a large sample program

-- question 4
intSum :: Int -> IO Int
intSum 0 = return 0
intSum n = do
  x <- getInt
  sum <- intSum (n - 1)
  return (x + sum)

getInt :: IO Int
getInt = do
  i <- getLine
  return (read i::Int)

adder :: IO ()
adder = do
  putStrLn "How many numbers?"
  n <- getInt
  result <- intSum n
  putStrLn ("Result is: " ++ show result)
  return ()

-- question 5
adder' :: IO ()
adder' = do
  putStrLn "How many numbers?"
  n <- getInt
  ns <- sequence [getInt | _ <- [0..n-1]]
  putStrLn ("Result is: " ++ (show . sum) ns)
  return ()
