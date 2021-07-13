import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

-- count the number of lowercase letters in a string
lowers :: String -> Int
lowers cs = length [ c | c <- cs, isAsciiLower c ]

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c == c']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

decode :: Int -> String -> String
decode n = encode (-n) -- could have been decode n cs = encode (-n) but passing cs is redundant! (Eta reduction?)

table :: [Float]
table =  [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0, 2.4, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral  n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o-e)^2/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack cs = decode factor cs
  where
    factor = head (positions (minimum  chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs cs

{-
> encode 3 "hello my name is clinton and i like rollercoasters"
"khoor pb qdph lv folqwrq dqg l olnh uroohufrdvwhuv"

> crach "khoor pb qdph lv folqwrq dqg l olnh uroohufrdvwhuv"
"hello my name is clinton and i like rollercoasters"
-}