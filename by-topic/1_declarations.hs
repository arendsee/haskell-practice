-- Haskell is statically typed, but uses type inference, so 
-- no need to explicitly specify type, usually.
a = 1 + 2

-- Strings are made with double quotes, not singles
s = "asdf"
t = "rtrtr"

-- They can be joined with the magic '++' operator
s ++ t

-- Similarly arrays can be joined
x = [1,4,6]
y = [7,9]
x ++ y

-- Division is doubly
1 / 3

-- Negation and booleans
not True

-- Weirdly, not equals is
5 /= 5
5 /= 6

-- Since we are functional and hate side effects and all that
-- there is no += and friends

-- this does something quite unexpected (from the Algol POV) it defines a
-- function, if you call it, it creates an infinite loop and incrementation ...
a = a + 1
