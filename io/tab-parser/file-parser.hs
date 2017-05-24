import System.Environment (getArgs)
import Data.List
import Data.List.Split

-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith $ cut' 5 
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------



---------------------------------------------------------------------
-- All the below code is mine ---------------------------------------

-- my first file processing trial
-- take the input file, and reverse the line order
f :: String -> String
f [] = []
f cs = (unlines . reverse . lines) cs

-- There is a pattern in the above code, splitting the textual input, processing
-- each element, and then recombining the processed elements. This patter can be
-- abstracted:

-- Divide distribute recombine composition
dpr :: a -> (a -> [b]) -> ([b] -> [c]) -> ([c] -> d) -> d
dpr cs d p r = (r . p . d) cs

-- This function does the same as f above
f' :: String -> String
f' [] = []
f' cs = dpr cs d p r
    where d = lines
          p = reverse
          r = unlines

-- It can be easily adapted to new situations, e.g. mimic the `head` command
head' :: String -> String
head' [] = []
head' cs = dpr cs d p r
    where d = lines
          p = (take 10)
          r = unlines

-- We are not limited to strings, either, e.g. mimic wc
wc' :: String -> String
wc' [] = []
wc' cs = dpr cs d p r
    where d = lines
          p = map length 
          r = (show . sum)
-- This value differs from wc since newlines are not counted

-- Reverse columns in TAB delimited file
colrev' :: String -> String
colrev' [] = [] 
colrev' cs = dpr cs d p r
    where d = lines
          p = map (intercalate "\t" . reverse . splitOn "\t")
          r = unlines


not_comment :: String -> Bool
not_comment [] = False
not_comment (x:xs) = if x == '#' then False else True

-- uncomment
uncomment' :: String -> String
uncomment' [] = [] 
uncomment' cs = dpr cs d p r
    where
        d = lines
        p = filter not_comment
        r = unlines

-- transpose the output
transpose' :: String -> String
transpose' [] = [] 
transpose' cs = dpr cs d p r
    where
        d = transpose . map (splitOn "\t") . lines
        p = id
        r = unlines . map (intercalate "\t")

-- take column i
cut' :: Int -> String -> String
cut' _ [] = [] 
cut' i cs = dpr cs d p r
    where
        d = transpose . map (splitOn "\t") . lines
        p = last . take i
        r = intercalate "\n"
