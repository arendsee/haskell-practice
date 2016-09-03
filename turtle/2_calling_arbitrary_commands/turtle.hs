#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
    shell "cowsay 'I can run anything from in here!'" empty
    dir <- pwd
    time <- datefile dir
    -- echo time -- doesn't work because time is not Text
    (putStrLn . show) time
    print time

-- pwd      :: MonadIO io => io Turtle.FilePath
-- datefile :: MonadIO io => Turtle.FilePath -> io UTCTime

-- show     :: Show a => a -> String
-- print    :: Show a => a -> IO ()
-- putStrLn :: String -> IO ()
-- echo     :: Text -> IO ()
