-- This code is from the hackage documentation fro Control.Monad.Except. The
-- code is from the site, verbatim initially (though I will adapt it as I
-- figure it out), the comments are mine.

import Control.Monad.Except

-- From Control.Monad.Except I use two functions:
-- catchError :: MonadError e m => m a -> (e -> m a) -> m a
-- throwError :: MonadError e m => a -> (e -> m a)

-- A general approach, and the one used in "Write you a Scheme" tutorial, is to
-- define a custom data type for the errors, implement its show message, then
-- link it into the ErrorMonad.

-- Define the types of errors we wish to handle 
data LengthError
  = EmptyString
  | StringTooLong Int -- Int stores the length of the string
  | OtherError String -- String stores the description of the problem

-- Format the error message
instance Show LengthError where
  show EmptyString = "The string was empty!"
  show (StringTooLong len) = "String to long: " ++ (show len) ++ " > 5"
  show (OtherError msg) = msg

-- Use the monad type constructor `Either LengthError`, with failure as Left
-- and passing as Right. We could use something else. I'll experiment with
-- that. 
type LengthMonad = Either LengthError

main = do
  putStrLn "Enter a string:"
  s <- getLine
  reportResult $ calculateLength s

-- catchError takes a function that may have thrown an error, and a constructor
-- for loading an error into the error monad, and returns an error monad. This
-- functions seems to be unnecessary here, it rewraps the error in the same
-- form it came in, but I suppose it is just for pedagogical purposes. Still,
-- it doesn't give me a good idea of how catchError is really used.
calculateLength :: String -> LengthMonad Int
calculateLength s = catchError (calculateLengthOrFail s) Left

-- Calculate length, wrap result in the Either monad
-- `throwError` is returns the error holding value
-- `return` loads the passing state into the monad
calculateLengthOrFail :: String -> LengthMonad Int
calculateLengthOrFail [] = throwError EmptyString
calculateLengthOrFail s
  | len > 5   = throwError (StringTooLong len)
  | otherwise = return len
  where len = length s

-- print results
reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = print $ "String length: " ++ (show len)
reportResult (Left e)    = print e
