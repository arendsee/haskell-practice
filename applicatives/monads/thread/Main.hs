import Control.Monad

-- | Passes along two possible states, for success and failure.  A failure
-- prevents future processing, propagating the final message.
data Thread a = Thread (Either String (String, a)) deriving(Show)
instance Monad Thread where
  return x = Thread (Right ("", x)) 
  -- concatenate logs (stderr) and transform result
  Thread (Right (e1, x)) >>= f = case f x of
    (Thread (Right (e2, x2))) -> Thread $ Right (e1 ++ e2, x2)
    r2 -> r2
  -- propagate failure
  Thread (Left e1) >>= _ = Thread (Left e1)

-- Due to the Applicative Monad Proposal, which requires all Monads be explicit
-- instances of Applicative andFunctor, just defining the monad 

instance Functor Thread where
  fmap = liftM

instance Applicative Thread where
  pure = return
  (<*>) = ap

trydivide :: Double -> Double -> Thread Double
trydivide y x
  | y == 0 = Thread (Left errmsg)
  | otherwise = Thread (Right (sucmsg, x / y)) where
  errmsg = "FAILURE: division by 0\n"
  sucmsg = unwords ["SUCCESS: divided ", show x, " by ", show y, "\n"]

main :: IO ()
main = do
  putStrLn $ show $ return 100 >>= trydivide 2 >>= trydivide 10
                               >>= trydivide 0 >>= trydivide 10

  putStrLn $ show $ return 100 >>= trydivide 2 >>= trydivide 10
