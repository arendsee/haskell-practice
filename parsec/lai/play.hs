import Text.Parsec
import Text.Parsec.String

play :: String -> Either ParseError Integer
play s = parse pmain "parameter" s

pmain :: Parser Integer
pmain = do
  x <- chainl1 pnum pplus
  eof
  return x

pnum = fmap read (many1 digit)

-- many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- digit :: Stream s m Char => ParsecT s u m Char
-- read :: Read a => String -> a
-- fmap :: (a -> b) -> f a -> f b
-- -------------------------------------------------------------
-- [Char] := String
-- many1 digit |- ParsecT s u m [Char]
--             := ParsecT s u m String

pplus = char '+' >> return (+)

main :: IO ()
main = print (play "45+3+1")
