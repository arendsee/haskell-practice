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

  -- equivalent to `ByteString.writeFile "zzz.dat" . encode $ f`
  DB.encodeFile "zzz.dat" f

  -- NOTE: we have to provide a type signature when reading binary
  f' <- DB.decodeFile "zzz.dat" :: IO FooBar

  putStrLn $ show f'

  -- We could provide the wrong data type
  f'' <- DB.decodeFile "zzz.dat" :: IO Int

  -- So we can happily cast the data to anything, which in this case is a valid
  -- integer.
  putStrLn $ show f''

  -- In other cases it might lead to a runtime error. In other words, this is a
  -- kind of scary thing to do and should be used with care.
