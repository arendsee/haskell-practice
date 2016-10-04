import Text.ParserCombinators.Parsec  --hiding ((<|>), many)
import Control.Applicative
import Control.Monad

-- example 1
matchTrue :: Parser String
matchTrue = String "true"

parse matchTrue "a test parser" "true"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

main = do
    print $ parse matchTrue "..." "true"
