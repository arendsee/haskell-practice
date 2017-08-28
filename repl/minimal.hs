import System.Console.Repline
import Control.Monad
import Control.Monad.IO.Class

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd = liftIO . print . interpret

byWord :: Monad m => WordCompleter m
byWord n = return []

opts :: [(String, [String] -> Repl ())]
opts = []

initRepl :: Repl ()
initRepl = return ()

repl :: IO ()
repl = evalRepl "> " cmd opts (Word byWord) initRepl

main :: IO ()
main = repl
