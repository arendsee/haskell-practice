{-# LANGUAGE OverloadedStrings #-}
-- ^ required to use strings literals where Text is needed

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.IO as TIO

import System.Process
import System.IO

{-

What do I need?
  
  run a program, split the output to stderr and stdout with exit code run
  filters on the result, which may fail, if they fail they write a message to
  stderr and prevent any further processing.

-}

type Stderr = String
type Stdout = String
data Entry = Entry Int Double T.Text

-- | Passes along two possible states, for success and failure.  A failure
-- prevents future processing, propagating the final error message and the
-- final data that was passed into the failing function.
data Thread a = Thread (Either (Stderr, a) (Stderr, a))
instance Monad Thread where
  return x = Thread (Right ("", x)) 
  -- concatenate logs (stderr) and transform result
  Thread (Right (e1, x)) >>= f = Thread $ case f x of
    (Thread (Left e2)) = Left (e2, x)
    (Thread (Right (e2, x2))) = Thread (e1 ++ e2, x2)
  -- propagate failure
  r >>= _ = r

priority :: AT.Parser Int
priority = do
  AT.char '<'  
  i <- AT.decimal
  AT.char '>'
  return i

time :: AT.Parser Double
time = do
  AT.char '['
  AT.skipSpace
  d <- AT.double
  AT.char ']'
  return d

message :: AT.Parser T.Text
message = AT.takeWhile (/= '\n')

entry :: AT.Parser Entry
entry = do
  p <- priority
  t <- time
  AT.skipSpace
  m <- message
  AT.skipSpace
  return (Entry p t m)

dmesg :: AT.Parser [Entry] 
dmesg = AT.many1 entry

prepResult :: [Entry] -> T.Text
prepResult = T.pack . show . length

process :: T.Text -> Either T.Text T.Text
process s = case AT.parseOnly dmesg s of
  Left err -> Left err
  Right entries -> Right (prepResult entries)

-- | if Left, print the message to STDERR
-- | if Right, print to STDOUT
handleResult :: Either T.Text T.Text -> IO ()

runCommand' :: Either Stderr (Stderr, a) -> (Either Stderr (Stderr, b))
runCommand' i o e
  | i == 0    = Right (e,o)
  | otherwise = Left e

eitherReadProcess :: FilePath -> [String] -> String -> Either Stderr (Stdout, Stderr)
eitherReadProcess cmd args sin = case readProcessWithExitCode cmd args sin of
  (0, s, e) -> Right (s,e) -- success
  (_, _, e) -> Left e      -- failure

main :: IO ()
main = do
  (exitCode, sout, serr) <- eitherReadProcess "dmesg" ["--raw"] ""

  handleResult $ process <$> TIO.hGetContents hout
