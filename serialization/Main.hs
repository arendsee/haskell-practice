{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import qualified Data.Binary as DB
import Data.Digest.Pure.MD5 (md5)

data FooBar = Foo Int Int | Bar String Double deriving(Generic,Show)

instance DB.Binary FooBar

main :: IO ()
main = do
  let f = Bar "yolo" 1.23
  let b = DB.encode f -- encode as lazy bytestring
  let d = md5 b
  putStrLn $ show f
  putStrLn $ show b
  putStrLn $ show d
