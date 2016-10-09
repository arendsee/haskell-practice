module MyMonad
(
    MyMonad(..)
) where

class MyMonad m where
    -- chain function (composition function?)
    (>>=) :: m a -> (a -> m b) -> m b
    -- inject function (closure function?), pure to monad
    return :: a -> m a
    -- lossy chain
    -- (>>) :: m a -> m b -> m b
    -- -- fail
    -- fail :: String -> m a
