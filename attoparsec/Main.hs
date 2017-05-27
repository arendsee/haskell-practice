import Data.ByteString as B

main :: IO ()
main = do
  s <- B.getContents
  print s
