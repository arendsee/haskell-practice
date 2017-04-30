module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

{- parse ::                                                          -}
{-     Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t => -}
{-     Text.Parsec.Prim.Parsec s () a ->                             -}
{-     SourceName                     ->                             -}
{-     s                              ->                             -}
{-     Either ParseError a                                           -}

{----- 
 - This example composes spaces and symbol to recognize the regular expression
 - / +\S/
 ----}
{- readExpr :: String -> String                                   -}
{- readExpr input = case parse (spaces >> symbol) "lisp" input of -}
{-     Left  err -> "No match: " ++ show err                      -}
{-     Right val -> "Found value"                                 -}
{-                                                                -}
{- spaces :: Parser ()                                            -}
{- spaces = skipMany1 space                                       -}

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal =
    Atom String                  |
    List [LispVal]               |
    DottedList [LispVal] LispVal |
    Number Integer               |
    String String                |
    Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
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

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr =
    parseAtom   <|>
    parseNumber <|>
    parseString
