#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

-- See Wadler, Philip. 1995. Monads for functional programming


---------------------------------------------------------------------
-- either a constant (Con) or a quotient (Div)
data Term = Con Int | Div Term Term

---------------------------------------------------------------------
-- identity monad

-- Wadley overloads the (*) operator, without bothering to tell us what he is
-- doing, I'll use (>>=) instead, since it is the Haskell standard.


---------------------------------------------------------------------
-- base case --
-- m >>= \a n

infixl 1  >>, >>=

type M a = a
unit :: a -> I a
unit a = a
(>>=) :: M a -> (a -> M b) -> M b
a >>= k = k a


eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = ((eval t) >>= (\a ((eval u) >>= (\b (unit (div a b))))))


---------------------------------------------------------------------
-- evaluation
main = do
    -- base case
    print $ eval (Div (Con 400) (Div (Con 200) (Con 10)) )  -- 20
