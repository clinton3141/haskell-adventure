import GHC.Show
import Prelude hiding (Maybe, Nothing, Just)

-- Types are aliases
type Pos = (Int,Int)

-- Data defines new types
data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x, y + 1)
move South (x,y) = (x, y - 1)
move East (x,y) = (x + 1, y)
move West (x,y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (flip move) p ms

-- data can also be made using Constructor functions
-- NOTE: Although Circle is a constructor function, it
-- differs from regular functions because there are no equations.
-- That is, Circle 5.0 cannot be reduced. Circle 5.0 is just data
-- in the same way that 5.0 is data.
data Shape = Circle Float | Rect Float Float
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Maybe a = Nothing | Just a deriving Show
saveDiv :: Int -> Int -> Maybe Int
saveDiv _ 0 = Nothing
saveDiv x y = Just (x `div` y)

-- newtype defines a new type with a single constructor
-- here Nat type is comprised of constructor function N
-- taking a single Int
--
-- This is a new type, so not an alias for, and cannot
-- be mismatched with Int
newtype Nat = N Int
