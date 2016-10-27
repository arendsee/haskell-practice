import Text.ParserCombinators.Parsec

-- take lines until none are left to take
csvFile = endBy line eol
line    = sepBy cell (char ',')
cell    = many (noneOf ",\n")
eol     = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


fastaFile = sepBy entry (char ',')
entry = many (noneOf ",\n")

-- parseFasta :: String -> Either ParseError [[String]]
-- parseFasta input = parse fastaFile "(unknown)" input
