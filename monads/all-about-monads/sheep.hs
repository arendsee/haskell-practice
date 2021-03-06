import Control.Monad

data Sheep = Sheep (Maybe Sheep) (Maybe Sheep) deriving(Show)

has :: Maybe Sheep -> String
has Nothing  = "None" 
has (Just _) = "Some"

mother :: Sheep -> Maybe Sheep
mother (Sheep Nothing  _) = Nothing
mother (Sheep (Just s) _) = Just s

father :: Sheep -> Maybe Sheep
father (Sheep _ Nothing ) = Nothing
father (Sheep _ (Just s)) = Just s

-- We could do this ..
{- maternalGrandmother :: Sheep -> Maybe Sheep       -}
{- maternalGrandmother s = case (mother s) of        -}
{-    Nothing -> Nothing                             -}
{-    Just x -> mother x                             -}
{-                                                   -}
{- mothersMaternalGrandmother s = case (mother s) of -}
{-    Nothing -> Nothing                             -}
{-    Just s' -> case (mother s') of                 -}
{-        Nothing -> Nothing                         -}
{-        Just s'' -> mother s''                     -}
-- But this is an abomination

-- Instead we can write a combinator:
{- comb' :: Maybe a -> (a -> Maybe b) -> Maybe b                                        -}
{- comb' Nothing _ = Nothing                                                            -}
{- comb' (Just x) f = f x                                                               -}
{-                                                                                      -}
{- mothersMaternalGrandmother :: Sheep -> Maybe Sheep                                   -}
{- mothersMaternalGrandmother s = (Just s) `comb'` mother `comb'` mother `comb'` mother -}
-- This is an informal monad

-- now we can use monadic sequences
{- mothersMaternalGrandmother :: Sheep -> Maybe Sheep                       -}
{- mothersMaternalGrandmother s = return s >>= mother >>= mother >>= mother -}

-- another advantage of having a monad be a formal member of the monad class is
-- that we can use `do` notation
{- main = do                                                                       -}
{-     m <- mother s                                                               -}
{-     m' <- father m                                                              -}
{-     m'' <- father m'                                                            -}
{-     return m''                                                                  -}



-- Exercise 1
maternalGrandmother s = return s >>= mother >>= mother
mothersMaternalGrandfather s = return s >>= mother >>= mother >>= father

-- Exercise 2
parent :: (MonadPlus m) => Sheep -> m Sheep
parent (Sheep m f) = m `mplus` f 

grandparent :: (MonadPlus m) => Sheep -> m Sheep
grandparent (Sheep m f) = (m >>= parent) `mplus` (f >>= parent)

ancestor :: Int -> Maybe Sheep -> Bool
ancestor 0 _ = True
ancestor _ Nothing = False
ancestor i (Just (Sheep m f)) = a m || a f where a = ancestor (i-1)

main = do
    let s = Sheep (Just (Sheep (Just (Sheep Nothing Nothing)) Nothing)) Nothing
    putStrLn $ has $ mother s
    putStrLn $ has $ father s
    putStrLn $ has $ maternalGrandmother s
    putStrLn $ has $ mothersMaternalGrandfather s
    putStrLn $ has $ parent s
    putStrLn $ has $ grandparent s
    putStrLn $ show $ ancestor 0 (Just s)
    putStrLn $ show $ ancestor 1 (Just s)
    putStrLn $ show $ ancestor 2 (Just s)
    putStrLn $ show $ ancestor 3 (Just s)
    putStrLn $ show $ ancestor 4 (Just s)
