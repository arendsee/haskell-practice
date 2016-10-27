import System.Environment (getArgs)
import Data.List
import Data.List.Split

-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith $ write'
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------


-- data GFF = GFF String String String Int Int Char Char Char [(String,String)]
data GFF = GFF String String String String String String String String String deriving Show

readGFF :: String -> Maybe GFF
readGFF [] = Nothing
readGFF ('#':_) = Nothing
readGFF s = case (words s) of
    [a,b,c,d,e,f,g,h,i] -> Just (GFF a b c d e f g h i)
    _ -> Nothing

-- Divide distribute recombine composition
dpr :: a -> (a -> [b]) -> ([b] -> [c]) -> ([c] -> d) -> d
dpr cs d p r = (r . p . d) cs

writeGFF :: Maybe GFF -> String
writeGFF Nothing = ""
writeGFF (Just (GFF a b c d e f g h i)) = intercalate "\t" [a,b,c,d,e,f,g,h,i]

write' :: String -> String
write' [] = []
write' cs = dpr cs d p r
    where d = lines
          p = map (writeGFF . readGFF)
          r = unlines
