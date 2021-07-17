-- Challenge 1: write id
id' :: a -> a
id' x = x

-- Challenge 2: write compose
compose :: (a -> b) -> (b -> c) -> a -> c
compose f g = g . f

-- Challenge 3: test compose respects id
testCompose :: Eq b => (a -> b) -> a -> Bool
testCompose f x = compose id' f x == compose f id' x
-- also :t compose id' id' :: a -> a

-- Challenge 4: Is the world-wide web a category in any sense
{-
If we take links to be morphisms then the web fails to be a category. A counter example
would be any page which does not link to itself. Also if A links to B, and B to C, we do
not necessarily have composition because A may not link to C.

If the morphism were defined as "can be reached from" we get close to a category.
The composition problem is solved, however id may still not be satisfied. Perhaps the
notion of being reached could be bent by saying that a page can reach itself by refreshing,
but I feel that this is pushing it.
-}

-- Challenge 5: Is Facebook a category with friendship as morphisms
{-
No: A is friends with B, and B is friends with C does not imply A is friends with C, hence
composition fails
-}

-- Challenge 6: When is a directed graph a category
{-
If a morphism is following an edge, then either:
1. when it is a complete digraph (including an edge from each node to itself)
2. when there are no edges other than one edge from each node to itself

If a morphism is reachability by following n edges, then:
1. when every node has an edge pointing to itself
-}