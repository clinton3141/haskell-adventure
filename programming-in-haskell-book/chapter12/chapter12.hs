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


