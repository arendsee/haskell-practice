-- I am roughly following LYAH here, by defining a typeclass for mapping 0, [],
-- "", etc to False.

class MultiBool a where
    multibool :: a -> Bool

instance MultiBool [a] where
    multibool [] = False 
    multibool [a] = True

instance MultiBool Int where
    multibool 0 = False
    multibool _ = True
    
instance MultiBool (Maybe a) where
    multibool (Just _) = True
    multibool Nothing  = False

instance MultiBool Bool where
    multibool True = True
    multibool False = False


-- functors
-- 
-- A type constructor
-- -----.
--      |
--      v
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap :: map

main = do
    let x = 0 :: Int
    (print (multibool []))
    (print (multibool x))
