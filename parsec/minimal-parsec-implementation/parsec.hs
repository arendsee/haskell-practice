-- Code based off (Leijen Meijer, 2001) - quoted sections, marked by
-- indentation, are quotes from their paper (unless otherwise noted)

-- although they didn't do it in their paper, I'll go ahead and make the parser
-- an explicit member of the Monad typeclass

import Data.Char
import Prelude hiding ((>>=), return)

type Parser a = String -> Consumed a

data Consumed a
  = Consumed (Reply a)
  | Empty    (Reply a)
  deriving Show

data Reply a
  = OK a String
  | Error
  deriving Show

return :: a -> Parser a
return x = \input -> Empty (OK x input)

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =
  -- String -> Consumed b
  \input -> case (p input) of
    Empty reply1
      -> case (reply1) of
        OK x rest -> (f x) rest
        Error -> Empty Error

    Consumed (reply1)
      -> Consumed
         (case (reply1) of
            OK x rest
              -> case ((f x) rest) of
                Consumed reply2 -> reply2
                Empty reply2 -> reply2
            Error -> Error
         )

(<|>) :: Parser a -> Parser a -> Parser a
f <|> g =
  \input -> case (f input) of
    Empty Error -> g input
    Empty ok -> case g input of
      Empty _ -> Empty ok
      consumed -> consumed
    consumed -> consumed

-- | Return Consumed if test passes, Empty otherwise
satisfy :: (Char -> Bool) -> Parser Char
satisfy test =
  \input -> case input of
    []     -> Empty Error
    (c:cs) | test c    -> Consumed (OK c cs)
           | otherwise -> Empty Error

-- With satisfy we can make lots of parsers, see all the nifty functions in
-- Data.Char, e.g. isUpper, isSpace, etc

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Char
digit = satisfy isDigit

ascii :: Parser Char
ascii = satisfy isLower

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- (many1 p <|> return [])
  return (x:xs)


main :: IO ()
main = do
  let s = "parse the hand"
  print s
  {- print $ many1 ascii s -}
  {- print $ char 's' >>= char 'h' -}
