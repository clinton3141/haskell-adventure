import Prelude hiding (Functor, Maybe, Just, Nothing, fmap, Applicative, (<*>), pure)

-- Functors are types which support a mapping function
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- here `f` is a _type parameter_.

instance Functor [] where
  fmap = map

data Maybe a = Just a | Nothing deriving Show

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- Functors can also wrap non container types. e.g. we can add fmap to IO
instance Functor IO where
  fmap g mx = do { x <- mx; return (g x) }
-- ghci> fmap show (return True) -- "True"

-- Functor Laws
-- fmap id = id
-- fmap (g . h) = fmap g . fmap h

-- these laws ensure that fmap actually performs a map operation
-- e.g. the following conforms to correct type, but does not
-- conform to the laws. So this is _NOT_ a functor
data MyList a = Nil | Cons a (MyList a) deriving Show
instance Functor MyList where
  fmap g Nil = Nil
  fmap g (Cons x ls) = Cons (g x) Nil

-- it turns out that there is at most one correct implementation
-- of functor for a parameterised type.



-- Applicatives generalise the idea of a functor to mappings of
-- more than one argument
-- This can be done using `pure` and `<*>`
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- fmap0 :: a -> f a
-- fmap0 = pure
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap1 f x = pure f <*> x
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 f x y = pure f <*> x <*> y
-- ...
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

-- hence Maybe can handle failure
-- ghci> pure (+) <*> Just 1 <*> Nothing -- Nothing
-- ghci> pure (+) <*> Just 1 <*> Just 2 -- Just 3
-- ghci> pure (\x -> \y -> \z -> x + y + z) <*> (Just 3) <*> Nothing <*> (Just 2) -- Nothing


-- with lists, each function is applied to each argument
instance Applicative [] where
  pure x = [x]
  gs <*> xs = [g x | g <- gs, x <- xs]
-- ghci> pure (+) <*> [1,2] <*> [2,3] -- [3,4,4,5]

-- Note that [a] is a generalisation of Maybe a that allows for multiple results in the case of success
-- So [] may be thought of as a failure, and non empty list represents all possible successes

-- get list of all products using a comprehension
products :: [Int] -> [Int] -> [Int]
products xs ys = [x * y | x <- xs, y <- ys]

-- but now we may do it without naming intermediate results
products' :: [Int] -> [Int] -> [Int]
products' xs ys = pure (*) <*> xs <*> ys