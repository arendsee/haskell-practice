import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as Atto
import Data.Word (Word8)

type Header = B.ByteString
type Sequence = B.ByteString

data Entry = Entry Header Sequence

isNewline :: Word8 -> Bool
isNewline c = c == 10 -- '\n'

isHeader :: Word8 -> Bool
isHeader c = c == 62  -- '>'

header :: Atto.Parser Header
header = do
  Atto.skip isHeader
  s <- Atto.takeWhile (not . isNewline)
  return s

bioseq :: Atto.Parser Sequence
bioseq = do
  s <- Atto.takeWhile (not . isHeader)
  return s

entry :: Atto.Parser Entry
entry = do
  h <- header
  q <- bioseq
  return $ Entry h q

fasta :: Atto.Parser [Entry]
fasta = Atto.many' entry


main :: IO ()
main = do
  s <- B.getContents
  es <- Atto.parse fasta s
  print es
