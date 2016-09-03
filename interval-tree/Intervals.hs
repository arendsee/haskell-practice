module Intervals
(
    Interval(..),
    overlap
) where

data Interval = Interval Int Int deriving (Show)

overlap :: Interval -> Interval -> Bool
overlap (Interval a b) (Interval c d) =
    (a >= c && a <= d) || (b >= c && b <= d)

mergeTwo :: Interval -> Interval -> Interval

merge :: [Interval] -> [Interval] 
