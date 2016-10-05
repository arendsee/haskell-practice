module Entry
(
    Entry(..)
    , Header(..)
    , Sequence(..)
    , readFasta
) where

import Data.List.Split

data Header = Header String String deriving Show

data Sequence = UNK String | AA String | DNA String | RNA String deriving Show

data Entry = Entry Header Sequence deriving Show

class Parse a where
    parse :: String -> a

instance Parse Entry where
    parse s = Entry header sequence where
        ss = lines s
        header   = (parse . unlines . take 1) ss
        sequence = (parse . unlines . drop 1) ss

instance Parse Header where
    parse s = Header name desc where
        w = words s
        name = (unwords . take 1) w
        desc = (unwords . drop 1) w

instance Parse Sequence where
     parse s = UNK s

readFasta :: String -> [Entry]
readFasta [] = []
readFasta xs = (map parse . splitRecords) xs where
    -- This function is not quite correct, entries should be split only if '>' is
    -- at the beginning of the line. It is perfectly legal inside a header.
    splitRecords :: String -> [String]
    splitRecords [] = []
    splitRecords s = (drop 1 . splitOn ">") s
