### Question 1

#### What is the type of: `['a', 'b', 'c']`

`[Char]`

#### What is the type of `('a', 'b', 'c')`

`(Char, Char, Char)`

#### What is the type of `[(False, '0'), (True, '1')]`

`[(Bool, Char)]`

#### What is the type of `([False, True], ['0', '1'])`

`([Bool], [Char])`

#### What is the type of `[tail, init, reverse]`

`[[a] -> [a]]`

### Question 2

What is the type of the following functions

#### `second xs = head (tail xs)`

`[a] -> a`

#### `swap (x, y) = (y, x)`

`(a, b) -> (b, a)` (ghci reports it as `(b, a) -> (a, b)`)

#### `pair x y = (x, y)`

`a -> b -> (a, b)`

#### `double x = x * 2`

`Num a => a -> a`

#### `palindrome xs = reverse xs == xs`

`Eq a => [a] -> Bool`

#### `twice f x = f (f x)`

` (a -> a) -> a -> a` (ghci reports it as `(t -> t) -> t -> t`)



### Self study on Type Classes
What is the type of
* `[(+), (*), div]`: `Integral a => [a -> a -> a]`
* `(/=)`: `Eq a => a -> Bool`
* `(<)`: `Ord a => a -> a-> Bool`
* `[(<), (/=)]`: `Ord a => [a -> a -> Bool]` (note that we use `Ord` here because all `Ord`s are `Eq`s
