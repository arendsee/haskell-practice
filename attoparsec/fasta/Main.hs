import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.Text as Text
import Data.Word (Word8)

type Header = B.ByteString
type Sequence = B.ByteString

data Entry = Entry Header Sequence deriving(Show)

isSeqChar :: Word8 -> Bool
isSeqChar c = (c >= 65 && c <= 90)  ||
              (c >= 97 && c <= 122) ||
              c == 10

header :: Atto.Parser Header
header = do
  Atto.skip (\c -> c == '>') -- '>'
  s <- Atto.takeWhile (\c -> c /= 10) -- '\n'
  return s

bioseq :: Atto.Parser Sequence
bioseq = do
  s <- Atto.takeWhile isSeqChar
  return s

entry :: Atto.Parser Entry
entry = do
  h <- header
  q <- bioseq
  return $ Entry h q

fasta :: Atto.Parser [Entry]
fasta = do 
  e <- Atto.many' entry
  return e

main :: IO ()
main = do
  s <- B.getContents
  -- NOTE: I use the `parseOnly` parser here instead of `parse` The reason is
  -- that `parse` returns a partial result, because the `takeWhile` in the last
  -- sequence never fails. The reaches the end of the file and still all
  -- characters have met the condition.
  print $ Atto.parseOnly fasta s
