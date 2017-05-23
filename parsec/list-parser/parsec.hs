import qualified Text.Parsec as Par
import qualified Text.Parsec.Token as Tok



main = do
  print $ parseExpr "[1,2,3]"
