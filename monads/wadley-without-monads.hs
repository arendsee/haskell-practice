#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

-- See Wadler, Philip. 1995. Monads for functional programming


---------------------------------------------------------------------
-- either a constant (Con) or a quotient (Div)
data Term = Con Int | Div Term Term


---------------------------------------------------------------------
-- base case --

eval :: Term -> Int
eval (Con a) = a
eval (Div a b) = div (eval a) (eval b)


---------------------------------------------------------------------
-- 1. handle exceptions

data M a = Raise Exception | Return a
type Exception = String

instance Show a => Show (M a) where
    show (Return a) = show a
    show (Raise  e) = show e

eval' :: Term -> M Int

eval' (Con a)   = Return a
eval' (Div x y) = case (eval' x) of
    Raise e -> Raise e                                  -- recursive catch in x
    Return a ->                                         -- if no lower error
        case (eval' y) of                                -- check y
            Raise e -> Raise e                          -- recursive catch in y
            Return b ->                                 -- if no lower errors
                if b == 0
                then
                    Raise "ERROR: Divide by 0"          -- raise error
                else                                    
                    Return (div a b)                    -- do division


---------------------------------------------------------------------
-- 2. hold state
-- with count of number of divisions performed --

-- type `M' a` is now a function that takes the inital state and returns
-- the computed value paired with the final state

type M' a =   State -> (a, State)
--              ^       ^   ^
--              |       |   |
--              /       /   |
-- initial state  _____/    /
--               /         /
--   output value         /
--                       /
--   final state _______/

type State = Int

str_Ma :: Show a => (a, State) -> String
str_Ma (a, s) = "(" ++ show a ++ ", " ++ show s ++ ")"

eval'' :: Term -> M' Int
eval'' (Con a) x = (a, x)
eval'' (Div t u) x = 
    let (a, y) = eval'' t x in
    let (b, z) = eval'' u y in
    (div a b, z + 1)


---------------------------------------------------------------------
-- 3. process output
-- tracing output

type M'' a = (Output, a)
type Output = String

showterm :: Term -> String
showterm (Con x) = "Con" ++ show x
showterm (Div x y) = "Div" ++ "(" ++ showterm x ++ ")(" ++  showterm y ++ ")"

str_Ma' :: (Output, a) -> String
str_Ma' (o, a) = o 

eval''' :: Term -> M'' Int
eval''' (Con a) = (line (Con a) a, a)
eval''' (Div t u) =
    let (x, a) = eval''' t in
    let (y, b) = eval''' u in
    (x ++ y ++ line (Div t u)(div a b), (div a b))

line :: Term -> Int -> Output
line t a = "eval(" ++ showterm t ++ ") <= " ++ show a ++ "\n"


---------------------------------------------------------------------
-- tracing output reverse
eval'''' :: Term -> M'' Int
eval'''' (Con a) = (line (Con a) a, a)
eval'''' (Div t u) =
    let (x, a) = eval'''' t in
    let (y, b) = eval'''' u in
    (line (Div t u)(div a b) ++ x ++ y, (div a b))



---------------------------------------------------------------------
-- Overview

-- In all three cases, `Term -> M Int`, in general `a -> M b`, where b is the
-- output and M contains additional effects.


---------------------------------------------------------------------
-- evaluation
main = do
    -- base case
    print $ eval (Div (Con 400) (Div (Con 200) (Con 10)) )  -- 20

    -- with warnings
    print $ eval' (Div (Con 400) (Div (Con 200) (Con 10)) )  -- 20
    print $ eval' (Div (Con 1) (Con 0))                      -- error

    -- with operation count
    putStrLn $ str_Ma $ eval'' (Div (Con 400) (Div (Con 200) (Div (Con 20) (Con 2))) ) 0  -- (20, 2)

    -- print trace
    putStr $ str_Ma' $ eval''' (Div (Con 400) (Div (Con 200) (Div (Con 20) (Con 2))) )

    -- reverse
    putStr $ str_Ma' $ eval'''' (Div (Con 400) (Div (Con 200) (Div (Con 20) (Con 2))) )
