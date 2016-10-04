module Intervals
(
    Interval(..),
    overlap
) where

-- create an alias (rather pointless here)
-- aliases are purely for readability, like typedef in C
type Position = Integer


-- Type constructor   
--          |              Data constructor
--         /          _________/
--        /          /
--       v          v
data Interval = Interval Position Position deriving (Show)
-- Type and data constructors needn't have the same name
-- The type constructor can be anything and will never be used again

overlap :: Interval -> Interval -> Bool
overlap (Interval a b) (Interval c d) =
    (a >= c && a <= d) || (b >= c && b <= d)

mergeTwo :: Interval -> Interval -> Interval

merge :: [Interval] -> [Interval] 
