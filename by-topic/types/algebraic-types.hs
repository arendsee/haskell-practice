-- define possible values for your data type
-- like the syntax for productions
data Few = 2 | 3 | 4
data Several = 3 | 4 | 5 | 6

-- defining the rules for lambda calculus, borrowing from Raul Rojas
-- http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
data Expression  = Name | Function | Application
data Function    = Name Expression
data Application = Expression Expression


-- a Point class type, with members in three dimensions
data Point = D Float | DD Float Float | DDD Float Float Float deriving(Show)

-- these types are functions, and as with all functions, can be underspecified
map (DDD 1.2 45.4) [1,2,3]

-- but what if we want to specify x and z, but not y?
-- (`:{...:}` allows multi-line input to ghci)
:{
data Point = D   { x :: Float } |
             DD  { x :: Float, y :: Float } |
             DDD { x :: Float, y :: Float, z :: Float }
             deriving(Show)
:}
-- now I can partially specify whatever I want
map (DDD {y=34, z=5}) [1,2,3]
-- oh, well, screw it all, that actually isn't allowed ...

data Maybe Thing = Nothing | Just Thing


-- ``inheritance'' - not really!!!
data Bool = True | False deriving(Read, Show)
a = True :: Bool
show a
read "True" :: Bool

-- Ord makes the possible values ordered
-- Ord requires Eq, it won't automatically import it for you
data A = Low | Middle | High deriving(Read, Show, Ord, Eq)
a = Low
b = High
a > b

-- No automagic here, just the first defined is ordered as lowest
data A' = High' | Low' | Middle' deriving(Read, Show, Ord, Eq)
a = Low'
b = High'
a > b

-- The example for LYAH is pretty good:
:{
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
:}
-- here Eq and Ord make the days ordered, so
a = Wednesday
b = Monday
b < a
-- Bounded type gives us access to minBound and maxBound variables:
minBound :: Day
maxBound :: Day
-- Enum allows succ and pred to work
succ b
-- You can succ to the end,
(succ . succ . succ . succ) a
-- but if you try to go past the bound, you die terribly
(succ . succ . succ . succ . succ) a
