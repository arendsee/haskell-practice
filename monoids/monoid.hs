-- A monoid is defined simply as a set with both an identity function and an
-- associative binary operator.

class MyMonoid a where
    -- identity element
    monoid_id :: a
    -- binary operator
    monoid_op :: a -> a -> a

data Clock = Clock Int Int deriving Show

instance MyMonoid Clock where
    monoid_id = (Clock 24 0)
    monoid_op (Clock h1 m1) (Clock h2 m2) = Clock (mod ht 24) (mod mt 60) where
        mt = m1 + m2
        ht = h1 + h2 + div mt 60

main = do
    let a = Clock 23 4
    let b = Clock 1 59
    print $ a
    print $ monoid_op a b
    print $ monoid_op a monoid_id
