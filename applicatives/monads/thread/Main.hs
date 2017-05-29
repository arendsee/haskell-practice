import Control.Monad
import Data.Monoid
import System.IO

-- | Passes along two possible states, for success and failure.  A failure
-- prevents future processing, propagating the final message.
data Thread e o = Thread (Either e (e, o))

instance Monoid e => Monad (Thread e) where
  return x = Thread (Right (mempty, x)) 
  -- concatenate logs (stderr) and transform result
  Thread (Right (e1, x)) >>= f = case f x of
    (Thread (Right (e2, x2))) -> Thread $ Right (e1 <> e2, x2)
    (Thread (Left e2)) -> Thread $ Left (e1 <> e2)
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
  show (Thread (Left e)) = show e ++ "\n *** FAILURE ***"
  show (Thread (Right (e, o))) = show e ++ "\n" ++ show o

data Log a = Log [(String, Maybe a)]

instance Monoid (Log a) where
  mempty = Log []
  mappend (Log x) (Log y) = Log (x <> y)

instance Show a => Show (Log a) where
  show (Log xs) =
    concatMap show' (zip [1..] xs) ++ "\n"
      ++ show (map snd xs) ++ "\n"
    where
      show' (i,(e,_)) = "-- Step " ++ show i ++ " - " ++ e ++ "\n"

trydivide' :: Double -> Double -> Thread (Log Double) Double
trydivide' y x
  | y == 0 = Thread (Left elog)
  | otherwise = Thread (Right (olog, result)) where
  result = x / y
  errmsg = concat [show x, " / ", show y, " = ERROR"]
  sucmsg = concat [show x, " / ", show y, " = ", show result]
  elog = Log [(errmsg, Nothing)]
  olog = Log [(sucmsg, Just result)]

writeResult :: (Show e, Show o) => Thread e o -> IO ()
writeResult (Thread (Left e)) = hPutStr stderr (show e)
writeResult (Thread (Right (e,o))) =
  hPutStr stderr (show e) >> hPutStrLn stdout (show o)

main :: IO ()
main = do
  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100 >>= trydivide' 2 >>= trydivide' 10
                           >>= trydivide' 0 >>= trydivide' 10

  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100 >>= trydivide' 2  >>= trydivide' 10
                           >>= trydivide' 10 >>= trydivide' 10
