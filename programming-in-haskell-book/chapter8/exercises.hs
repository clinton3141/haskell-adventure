import Prelude hiding (Maybe, Just, Nothing)

-- Question 1
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ Zero) = m
mult m (Succ n) = add m (mult m n)

-- Question 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)             = x == y
occurs x (Node left y right) = case compare x y of
                                LT -> occurs x left
                                EQ -> True
                                GT -> occurs x right

-- Question 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show
leaves :: Tree' a -> Int
leaves (Leaf' a) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' a) = True
balanced (Node' l r) = balanced l &&
                        balanced r &&
                        abs (leaves l - leaves r) <= 1
-- Question 4
balance :: [a] -> Tree' a
balance [] = error "Cannot create tree from empty list"
balance [a] = Leaf' a
balance as = Node' (balance bs) (balance cs) where
            (bs, cs) = splitAt (length as `div` 2) as

-- Question 5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Question 6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y

-- Question 7
data Maybe' a = Nothing | Just a
instance Eq a => Eq (Maybe' a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

-- not sure how to hide this one...
-- instance Eq a => Eq [a] where
--   [] == [] = True
--   x:xs == y:ys = x == y && xs == ys

-- Question 8, Question 9 - in files in gitignore due to
-- requiring extensive code samples from the book