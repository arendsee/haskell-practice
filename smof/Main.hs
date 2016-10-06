import System.Environment (getArgs)
import Entry


-- Borrowed verbatim from Real World Haskell ------------------------
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith $ subcmd
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
---------------------------------------------------------------------

cmd :: ( String  -> [Entry] ) ->    -- d divide          1 -> m
       ( [Entry] -> [Entry] ) ->    -- f filter          m -> n
       ( Entry   -> a       ) ->    -- m map transform   n -> n
       ( a       -> b       ) ->    -- j join            n -> p
       ( b       -> String  ) ->    -- s stringify       p -> 1
cmd d f m j s = (s . j . map m . f . d)

----------- Smof subcommands ----------------------------------------
-- cut emulates UNIX cut command, where fields are entries
cut' :: String -> String
cut' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- clean cleans fasta files
clean' :: String -> String
clean' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- filter extracts sequences meeting the given conditions
filter' :: String -> String
filter' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- grep roughly emulates the UNIX grep command
grep' :: String -> String
grep' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- md5sum calculate an md5 checksum for the input sequences
md5sum' :: String -> String
md5sum' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- head writes the first sequences in a file
head' :: String -> String
head' x = (cmd d f m j s) x where
    d = readFasta
    f = take 1
    m = write
    j = id
    m = unlines

-- permute randomly order sequence
permute' :: String -> String
permute' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- reverse reverse each sequence (or reverse complement)
reverse' :: String -> String
reverse' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- sample randomly select entries from fasta file
sample' :: String -> String
sample' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- sniff extract info about the sequence
sniff' :: String -> String
sniff' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- sort sort sequences
sort' :: String -> String
sort' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- split split a fasta file into smaller files
split' :: String -> String
split' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- stat calculate sequence statistics
stat' :: String -> String
stat' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- subseq extract subsequence from each entry (revcomp if a<b)
subseq' :: String -> String
subseq' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- tail writes the last sequences in a file
tail' :: String -> String
tail' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- uniq count, omit, or merge repeated entries
uniq' :: String -> String
uniq' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines

-- wc roughly emulates the UNIX wc command
wc' :: String -> String
wc' x = (cmd d f m j s) x where
    d = readFasta
    f = id
    m = write
    j = id
    m = unlines
