main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn line
