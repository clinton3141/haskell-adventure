import System.IO

-- interactive programs are viewed as a pure function that takes
-- "state of the world" as an additional argument, and returns
-- "new state of the world" as an additional return value

-- a "like this but not really" example
data World
type IO' a = World -> (a, World)

-- getChar :: IO Char
-- putChar :: Char -> IO ()

-- the `return` function provides a bridge from pure functions to the
-- impure world. But note! There's no way back! Once impure, always impure
-- return :: a -> IO a

impure :: IO String
impure = do
  x <- getChar
  getChar
  return ("you typed " ++ [x])

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n' then
    return []
  else
    do
      xs <- getLine'
      return (c:xs)
