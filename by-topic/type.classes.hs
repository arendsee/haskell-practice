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

main = do
    let x = 0 :: Int
    (print (multibool []))
    (print (multibool x))
