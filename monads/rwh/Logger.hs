module Logger
    (
      -- A handle, no value constructors, only for use in type definitions
      -- e.g. panmorphic
      Logger
      -- holder for monad output
    , Log
      -- unwrap the monad
    , runLogger
      -- client code function for monad invocations
    , record
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
 
instance Functor Logger where
    fmap = liftM
 
instance Applicative Logger where
    pure = \a -> Logger (a, [])
    (<*>) = ap

-- wrapped in the arcana as required for publication

-- track the log message and function count
type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    (>>=) m k = let (a, (i, w)) = execLogger m
                    n           = k a
                    (b, x) = execLogger n
                in Logger (b, w ++ x)

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])
