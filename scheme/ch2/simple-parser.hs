module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric -- used for readOct and readHex

data LispVal =
    Atom String                  |
    List [LispVal]               |
    DottedList [LispVal] LispVal |
    Number Integer               |
    String String                |
    Bool Bool deriving(Show)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: "    ++ show err
    Right val -> "Found value: " ++ show val

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
    char '"'
    return $ String x

{- <|> means logical OR -}
{- #t and #f are true and false in Scheme -}
parseAtom :: Parser LispVal
parseAtom = do
    first <-        symbol <|> letter
    rest  <- many $ symbol <|> letter <|> digit
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

{- parseNumber :: Parser LispVal                     -}
{- parseNumber = liftM (Number . read) $ many1 digit -}

parseNumber :: Parser LispVal
parseNumber = parseNum <|> parseDec <|> parseHex <|> parseOct

parseNum =
    liftM (Number . read) $ many1 digit

parseDec = do
    char '#'
    char 'd'
    x <- many1 digit
    return $ Number $ read $ x

parseHex = do
    char '#'
    char 'x'
    x <- many1 (digit <|> oneOf "abcdef")
    return $ Number $ fst $ head $ readHex $ x

parseOct = do
    char '#'
    char 'o'
    x <- many1 (oneOf "01234567")
    return $ Number $ fst $ head $ readOct $ x

{- parseNumber = do               -}
{-     x <- many1 digit           -}
{-     return $ Number $ read $ x -}

parseExpr :: Parser LispVal
parseExpr =
    parseAtom   <|>
    parseNumber <|>
    parseString
