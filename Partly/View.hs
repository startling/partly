module Partly.View where
-- base:
import Control.Applicative
-- bytestring:
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary (get)
import Data.Binary.Get
-- aeson:
import Data.Aeson
-- aeson-pretty:
import Data.Aeson.Encode.Pretty
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json

data ViewBootRecord = ViewBootRecord
  { asJson :: Bool
  , uglify :: Bool
  , input  :: Maybe FilePath
  , output :: Maybe FilePath }
  deriving (Eq, Show)

viewOptions :: Parser ViewBootRecord
viewOptions = ViewBootRecord
  <$> switch
      ( long "json"
      & short 'j'
      & help "Show the boot record as JSON." )
  <*> switch
      ( long "ugly"
      & short 'u'
      & help "Don't prettify the JSON before writing it." )
  <*> argument (Just . Just)
      ( help "The file to parse and inspect; defaults to stdin."
      & metavar "input"
      & value Nothing )
  <*> maybeOption
      ( long "output"
      & value Nothing
      & short 'o'
      & help "A file to write to."
      & metavar "file" )
  where
    maybeOption :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
    maybeOption m = nullOption $ reader (Just . Just) & m

view :: ViewBootRecord -> IO ()
view (ViewBootRecord j u i o) = do
  mbr <- runGet (get :: Get BootRecord) <$> maybe L.getContents L.readFile i
  let json = encoder mbr in case (j, o) of
    (False, Nothing) -> print mbr
    (False, Just f) -> writeFile f $ show mbr
    (True, Nothing) -> L.putStr json
    (True, Just f) -> L.writeFile f json
  where
    encoder :: ToJSON j => j -> L.ByteString
    encoder = if u then encode else encodePretty

