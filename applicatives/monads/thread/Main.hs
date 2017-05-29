import Control.Monad
import Data.Monoid

-- | Passes along two possible states, for success and failure.  A failure
-- prevents future processing, propagating the final message.
data Thread e o = Thread (Either e (e, o))
instance Monoid e => Monad (Thread e) where
  return x = Thread (Right (mempty, x)) 
  -- concatenate logs (stderr) and transform result
  Thread (Right (e1, x)) >>= f = case f x of
    (Thread (Right (e2, x2))) -> Thread $ Right (e1 <> e2, x2)
    r2 -> r2
  -- propagate failure
  Thread (Left e1) >>= _ = Thread (Left e1)

-- Due to the Applicative Monad Proposal, which requires all Monads be explicit
-- instances of Applicative andFunctor, just defining the monad 

instance Monoid e => Functor (Thread e) where
  fmap = liftM

instance Monoid e => Applicative (Thread e) where
  pure = return
  (<*>) = ap

instance (Show e, Show o) => Show (Thread e o) where
  show (Thread (Left err)) = show err ++ "\n *** FAILURE ***"
  show (Thread (Right (_, out))) = show out

trydivide :: Double -> Double -> Thread String Double
trydivide y x
  | y == 0 = Thread (Left errmsg)
  | otherwise = Thread (Right (sucmsg, x / y)) where
  errmsg = "ERROR: division by 0\n"
  sucmsg = unwords ["SUCCESS: divided ", show x, " by ", show y, "\n"]

main :: IO ()
main = do
  putStrLn $ show $ return 100 >>= trydivide 2 >>= trydivide 10
                               >>= trydivide 0 >>= trydivide 10

  putStrLn $ show $ return 100 >>= trydivide 2 >>= trydivide 10
