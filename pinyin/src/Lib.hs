{-# LANGUAGE PartialTypeSignatures #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy.Char8   as BSL
import qualified Codec.Compression.GZip as GZip
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Attoparsec.Text   as Atto

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Uncompress CEDICT input file, and return a lazy list of lines.
readCEDICTLines :: FilePath -> IO [Text.Text] -- Text.Text
readCEDICTLines =
  fmap (fmap (Text.decodeUtf8 . BSL.toStrict) . BSL.lines . GZip.decompress) . BSL.readFile

skipMetadata = dropWhile (("#" ==) . Text.head)

data CEDICTRecord = CEdictRecord {
    simplifiedEntry
  , traditionalEntry
  , pinyin   :: Text
  , glossary :: Gloss
  }

type Gloss = Text -- temporary

-- Structure of the record:
-- simplified <space> traditional <space> [ <pinyin> ] / (tag) gloss; ... / gloss; .../
parseRecord = entryName <*> (space *> entryName) <*> space 
  where
    pinyin    = Atto.char '[' *> many (Atto.notChar ']') <* Atto.char ']'
    glosses     = Atto.char '/' *> Atto.sepBy1 gloss (Atto.char '/')
    gloss     = (Atto.many $ Atto.notChar '/')
    entryName = Atto.many Atto.notSpecial
    space     = Atto.char ' '
    notSpecial = Atto.notInClass "/[]()"
    rest:glossPart = Text.breakWith '/'