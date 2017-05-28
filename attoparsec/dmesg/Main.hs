import System.Process
import System.IO

getOut :: FilePath -> [String] -> IO (Maybe Handle)
getOut cmd args = 
  fmap ( \(_,x,_,_) -> x ) $
  createProcess (proc cmd args){ std_out = CreatePipe }

main :: IO ()
main = do
  Just hout <- getOut "ls" []

  -- I suppose this is kind of an obvious place to use bind, but this is not
  -- yet intuitive to me. But I can break down the problem, and the solution is
  -- just a matter of messing around with types. Set goals, find the puzzle
  -- pieces.
  --
  -- hout :: Handle
  --
  -- ??? Handle -> IO ()
  --
  -- hGetContents :: Handle -> IO String
  -- -----------------------------------
  --
  -- ??? IO String -> IO ()
  --
  -- (>>=) :: f a         -- 1
  --       -> (a -> f b)  -- 2
  --       -> f b         -- 3
  -- putStr :: String -> IO ()
  -- ---------------------------------
  --   let 1 be
  --      hGetContents hout
  --   let 2 be
  --      putStr
  -- ---------------------------------
  -- (>>=) :: IO String
  --       -> (String -> IO ())
  --       -> IO ()

  hGetContents hout >>= putStr
