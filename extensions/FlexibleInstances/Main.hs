{-# LANGUAGE FlexibleInstances #-}

class Doer a where
    doer :: a -> a

-- legal in vanilla
instance Doer Int where
    doer x = x + 1

-- illegal
-- Why? in vanilla, the number of non-generic types and constructors in the
-- head must be \ge to the number in the instance.
instance Doer [Int] where
    doer xs = fmap (+2) xs
-- whereas, this would be legal in vanilla:
-- `instance Doer [a] where ...`

main = do
    let x = 2::Int
    print (doer x)
    -- requires FlexibleInstances extension
    let xs = [1,2,3]::[Int]
    print (doer xs)
