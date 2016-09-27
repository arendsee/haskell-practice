import System.Environment (getArgs)

-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith wc'
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
