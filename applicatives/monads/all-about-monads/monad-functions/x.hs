import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

-- filterM
-- mapM    - return results as a list
-- mapM_   - run the given function for its side effects

main :: IO ()

---- 1 ------------------------------------------------------------------------
{- main = do                                    -}
{-     names <- getArgs                         -}
{-     dirs <- filterM doesDirectoryExist names -}
{-     mapM_ putStrLn dirs                      -}



---- 2 ------------------------------------------------------------------------
{- main = getArgs >>= mapM_ putStrLn -}



---- 3 ------------------------------------------------------------------------
-- but now I want to be able to recursively list, descending into directories

main = getArgs                 >>=
       (flip findFiles) "x.hs" >>=
       mapM getPermissions     >>=
       mapM_ (putStrLn . show . writable)



{- getPermissions :: FilePath -> IO Permissions       -}
{- writable :: Permissions -> Bool                    -}
{- readable :: Permissions -> Bool                    -}
{- searchable :: Permissions -> Bool                  -}
{- findFiles :: [FilePath] -> String -> IO [FilePath] -}
{- listDirectory :: FilePath -> IO [FilePath]         -}
{-                                                    -}
{- combine :: FilePath -> FilePath -> FilePath        -}
{- joinPath :: [FilePath] -> FilePath                 -}
