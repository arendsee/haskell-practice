module Main where
import System.Environment
main :: IO ()
main = do
    print "Enter value x: "
    {- bind: pull String out of the IO monad -}
    x <- getLine
    print "Enter value y: "
    y <- getLine
    print $ "x + y = " ++ ( show $ (read x) + (read y) )
{- main = do args <- getArgs                                             -}
{-           putStrLn ( show $ (read (args !! 0)) + (read (args !! 1)) ) -}
