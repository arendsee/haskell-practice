-- This is an example borrowed from
-- https://tweag.github.io/HaskellR/docs/differences-repl-source.html

{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

hello :: String -> R s ()
hello name = do
    [r| print(s_hs) |]
    return ()
  where
    s = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Name?"
    name <- getLine
    R.withEmbeddedR R.defaultConfig $ R.runRegion $ hello name
