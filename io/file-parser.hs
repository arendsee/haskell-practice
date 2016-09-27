import System.Environment (getArgs)

-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith f 
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------



---------------------------------------------------------------------
-- All the below code is mine ---------------------------------------

-- take the input file, and reverse the line order
f :: String -> String
f [] = []
f cs = (unlines . reverse . lines) cs
