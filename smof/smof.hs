import System.Environment (getArgs)
import Data.List
import Data.List.Split


-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith $ (show . head' . splitSeqs)
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------

data Header = Header String String deriving Show

data Sequence = UNK String | AA String | DNA String | RNA String deriving Show

data Entry = Entry Header Sequence deriving Show

splitRecords :: String -> [String]
splitRecords [] = []
splitRecords s = splitOn ">" s

makeHead :: String -> Header
makeHead s = case (words s) of
    []     -> Header "" ""
    (x:xs) -> Header x (unwords xs)

makeBody :: [String] -> Sequence
makeBody [] = UNK ""
makeBody s = UNK (unlines s)

extractSeq :: String -> Entry
extractSeq s = case (lines s) of
    (x:xs) -> Entry (makeHead x) (makeBody xs)

splitSeqs :: String -> [Entry]
splitSeqs [] = []
splitSeqs xs = (map extractSeq . splitRecords) xs

head' :: [Entry] -> Maybe Entry
head' [] = Nothing
head' (x:xs) = Just x


{- -- parallel processes, one entry to one result, 1-n-n-1                                       -}
{- -- NOTE: this also allows 1-n-m-1, i.e. lengths of [FastaEntries] needn't equal length of [a] -}
{- smof_p :: String -> (String -> [Entry]) -> ([Entry] -> [a]) -> ([a] -> b) -> b                -}
{- smof_p cs d p r = (r . p . d) cs  -- same as dpr, but less polymorphic                        -}
{-                                                                                               -}
{- -- linear processes, all entries to one result, 1-n-1                                         -}
{- smof_l :: String -> (String -> [Entry]) -> ([Entry] -> a) -> a                                -}
{- smof_l cs d p = (d . p) cs                                                                    -}
