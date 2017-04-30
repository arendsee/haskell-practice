module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric -- used for readOct and readHex

data LispVal =
    Atom String                  |
    Char Char                    |
    Number Integer               |
    Real Float                   |
    {- Rational Integer Integer     | -}
    {- Complex Num Num              | -}
    List [LispVal]               |
    DottedList [LispVal] LispVal |
    String String                |
    Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom s)          = s
showVal (Char s)          = show s
showVal (Number s)        = show s
showVal (Real s)          = show s
showVal (List ss)         = "(" ++ unlist ss ++ ")"
showVal (DottedList ss s) = "(" ++ unlist ss ++ " . " ++ showVal s ++ ")"
showVal (String s)        = "\"" ++ s ++ "\""
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"

unlist :: [LispVal] -> String
unlist = unwords . map showVal

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: "    ++ show err
    Right val -> "Found value: " ++ show val


-- Miscellaneous parsers --------------------------------------------

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

-- LispVal parsers --------------------------------------------------
  
parseExpr :: Parser LispVal
parseExpr =
    parseString   <|>
    parseReal     <|>
    {- parseRational <|> -}
    {- parseComplex  <|> -}
    parseNumber   <|>
    parseChar     <|>
    parseAtom     <|>
    parseQuoted   <|>
    do
        char '('
        x <- (try parseList) <|> parseDottedList
        char ')'
        return x

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
    char '"'
    return $ String x

{- <|> means logical OR -}
{- #t and #f are true and false in Scheme -}
parseAtom = do
    first <-        symbol <|> letter
    rest  <- many $ symbol <|> letter <|> digit
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

parseChar = try(
    do
        char '#' >> char '\\'
        x <- anyChar -- extend to \n, \r, \t ...
        return $ Char x
    )

parseReal = try(
    do
        left <- many1 $ digit
        char '.'
        right <- many1 $ digit 
        return $ Real $ read $ left ++ "." ++ right
    )

parseNumber = parseNum <|> parseDec <|> parseHex <|> parseOct

parseNum =
    liftM (Number . read) $ many1 digit

parseDec = try(
    do
        char '#' >> char 'd'
        x <- many1 digit
        return $ Number $ read $ x
    )

parseHex = try(
    do
        try (char '#' >> char 'x')
        x <- many1 (digit <|> oneOf "abcdef")
        return $ Number $ fst $ head $ readHex $ x
    )

parseOct = try(
    do
        try (char '#' >> char 'o')
        x <- many1 (oneOf "01234567")
        return $ Number $ fst $ head $ readOct $ x
    )

parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
