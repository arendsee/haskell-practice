import System.IO

main = do
  -- openFile :: FilePath -> IOMode -> IO Handle
  --   where
  --   data IOMode
  --     = ReadMode
  --     | WriteMode
  --     | AppendMode
  --     | ReadWriteMode
  --   type FilePath = String
  h <- openFile "Main.hs" ReadMode

  fsize <- hFileSize h
  freadable <- hIsReadable h

  -- print the file size
  print $ fsize
  print $ freadable

  hClose h

  -- Although, it is usually easier to use readFile, writeFile and appendFile
