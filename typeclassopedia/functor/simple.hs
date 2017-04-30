data Foo = Foo Int deriving Show

{- class MyFunctor f where             -}
{-     fmap' :: (a -> a) -> f a -> f a -}
{-                                     -}
{- instance MyFunctor Foo where        -}
{-     fmap' g (Foo x) = Foo (g x)     -}
{-                                     -}
{- doit :: Int -> Int                  -}
{- doit x = x + 2                      -}
