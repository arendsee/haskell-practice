import System.Environment (getArgs)

-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith reverse'
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------

data Header = Seqid Description

data Sequence = Protein | DNA | RNA

data Entry = Header Sequence

-- parallel processes, one entry to one result, 1-n-n-1
-- NOTE: this also allows 1-n-m-1, i.e. lengths of [FastaEntries] needn't equal length of [a]
smof_p :: String -> (String -> [Entry]) -> ([Entry] -> [a]) -> ([a] -> b) -> b
smof_p cs d p r = (r . p . d) cs  -- same as dpr, but less polymorphic

-- linear processes, all entries to one result, 1-n-1
smof_l :: String -> (String -> [Entry]) -> ([Entry] -> a) -> a
smof_l cs d p = (d . p) cs
