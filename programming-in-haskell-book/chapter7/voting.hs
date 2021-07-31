import Data.List

-- first past the post
votes :: [String]
votes = ["Oblivion", "Nemesis", "Galactica", "Nemesis", "Nemesis", "Oblivion"]

count :: Eq a => a -> [a] -> Int
count x = length . filter(== x)

removeDups :: Eq a => [a] -> [a]
removeDups []     = []
removeDups (x:xs) = x : filter(/= x) (removeDups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v)| v <- removeDups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- alternative vote
ballots :: [[String]]
ballots =  [["Nemesis", "Oblivion"],
            ["Galactica"],
            ["Oblivion", "Nemesis", "Galactica"],
            ["Galactica", "Oblivion", "Nemesis"],
            ["Oblivion"]]

removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter(/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map(filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (removeEmpty bs) of
              [c] -> c
              (c:cs) -> winner' (elim c bs)
