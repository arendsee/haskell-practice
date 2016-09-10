-- this is code borrowed from Derek White's presentation
-- https://www.youtube.com/watch?v=3q8xYFDYLeI

module Failure where

-- Problem - dies on `divBy 0` without running the rest

divBy :: Int -> Int -> Err Int
divBy 0 y = Error
divBy x y = OK (div y x)

data Err a = OK a | Error deriving Show

----- test code -----
main = do
    print $ (divBy 2 `composeError` divBy 5) 2310
    print $ (divBy 0 `composeError` divBy 7) 2310
    print $ (divBy 2 `composeError` divBy 11) 2310

-- This is filling in the transitive gap
--  a ------> b -> c
--    \       \
--     v       v
--     err(b)   err(c)
-- f(h . g) x :: (b->c) -> (a->b) -> (a->c)
composeError :: (b -> Err c) -> (a -> Err b) -> (a -> Err c)
composeError f g x = case g x of 
                     OK y  -> f y 
                     Error -> Error
