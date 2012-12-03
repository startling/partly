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

data ViewJsonOptions = ViewJsonOptions
  { uglify :: Bool
  , output :: Maybe FilePath
  , input  :: FilePath }
  deriving (Eq, Show)

viewJsonOptions :: Parser ViewJsonOptions
viewJsonOptions = ViewJsonOptions
  <$> switch
      ( long "ugly"
      & short 'u'
      & help "Don't prettify the JSON before writing it." )
  <*> maybeOption
      ( long "output"
      & short 'o'
      & help "A file to write to; defaults to stdout."
      & metavar "file" )
  <*> argument str
      ( help "The file to parse and inspect."
      & metavar "input" )
  where
    maybeOption :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
    maybeOption m = nullOption $ reader (Just . Just) & m & value Nothing

viewJson :: ViewJsonOptions -> IO ()
viewJson (ViewJsonOptions u o i) = do
  mbr <- runGet (get :: Get BootRecord) <$> L.readFile i
  writer $ encoder mbr
  where
    encoder = if u then encode else encodePretty
    writer = maybe L.putStr L.writeFile o

data ViewCommand
  = ViewJson ViewJsonOptions
  deriving (Eq, Show)

viewParser :: ParserInfo ViewCommand
viewParser = info
  ( subparser
    ( command "json"
      ( info (ViewJson <$> viewJsonOptions )
        ( progDesc "Read a boot record into JSON." ))))
  ( progDesc "Inspect a boot record."
  & fullDesc )

view :: ViewCommand -> IO ()
view c = case c of ViewJson vj -> viewJson vj;
