-- This is a modification of the example from Stephen Diehl's repline package

module Main where

import System.Console.Repline
import Control.Monad.State.Strict
import Data.List (isPrefixOf)


type Repl a = HaskelineT IO a

cmd :: String -> Repl()
cmd line = liftIO $ print line

nargs :: [String] -> Repl ()
nargs  = liftIO . putStrLn . show . length

nchars :: [String] -> Repl ()
nchars = liftIO . putStrLn . show . length . concat

opts :: [(String, [String] -> Repl ())]
opts = [("nargs", nargs), ("nchars", nchars)]

byWord :: Monad m => WordCompleter m
byWord n = do
  let names = ["psychodefroculate", ":nchars", ":nargs"]
  return $ filter (isPrefixOf n) names

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":nargs" , fileCompleter)
  , (":nchars" , listCompleter ["lollipop", "kitchen sink"])
  ]

initRepl :: Repl () 
initRepl = return ()

repl :: IO ()
repl = evalRepl "repl> " cmd opts (Prefix (wordCompleter byWord) defaultMatcher) initRepl

main :: IO ()
main = repl
