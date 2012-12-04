module Partly.Make where
-- base:
import Data.Word
import Control.Applicative
-- bytestring:
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary (put)
import Data.Binary.Put
-- aeson:
import Data.Aeson
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json

data MakeOptions = MakeOptions
  { from   :: Maybe FilePath
  , btl    :: Maybe FilePath
  , json   :: Bool
  , output :: Maybe FilePath }
  deriving (Eq, Show)

makeOptions :: Parser MakeOptions
makeOptions = MakeOptions
  <$> maybeOption
    ( long "from"
    & short 'f'
    & metavar "file"
    & help "A bootloader to base this one on, either binary or JSON." )
  <*> maybeOption
    ( long "bootloader"
    & short 'b'
    & metavar "file"
    & help "A file to include as a bootloader." )
  <*> switch
    ( long "json"
    & short 'j'
    & help "Output the MBR as JSON rather than a binary blob." )
 <*> maybeOption
    ( long "output"
    & short 'o'
    & metavar "file"
    & help "Write to a file rather than stdout." )
  -- TODO: should "output" be required, so that "partly make" gives you
  -- a help screen?
  where
    maybeOption :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
    maybeOption m = nullOption $ reader (Just . Just) & m & value Nothing

makeParser :: ParserInfo MakeOptions
makeParser = info makeOptions
  ( progDesc "Create an MBR, potentially based on some existing one."
  & fullDesc)

make :: MakeOptions -> IO ()
make m = return ()
