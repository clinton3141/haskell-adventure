{-

cons is an operator.

> :t (:) :: a -> [a] -> [a]

So to use it in pattern matching we must parenthesise it, because function application
has highest precedence. `startsWithA 'a':_` means `(startsWithA 'a'):_`
-}

startsWithA :: [Char] -> Bool
startsWithA ('a':_) = True
startsWithA _ = False

-- and from prelude

null :: [a] -> Bool
null [] = True
null (_:_) = False
-- could also be null _ = False

-- using guarded equations
startsWithA' cs | head cs == 'a' = True
                | otherwise = False



{-
Lambdas
-}

-- can be used just like Lambda calc (but don't do this!)
two = (\x -> x + 1) 1

-- more usefully
evens = map (\x -> x `mod` 2 == 0) [1..10] -- but really `evens = map even [1..10]`

{-

Operators and sections

An operator is an infix function - for example +.

A prefix function can be used infix by using backticks - `div 1 2` becomes '1 `div` 2.

Likewise, an infix operator can be converted to a prefix. In this case by surrounding it
in parens:

(+) 1 2

When an argument is partially applied to an operator, it is called a "section".

(1 +) is a "left section" and we have (1+) :: Num a => a -> a

(+ 1) is a "right section"

(+ 1) 1 == 2

@see https://wiki.haskell.org/Section_of_an_infix_operator

-}

product' = foldr (*) 1