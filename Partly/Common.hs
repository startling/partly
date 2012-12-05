module Partly.Common where
-- base:
import Control.Applicative
-- bytestring:
import qualified Data.ByteString.Lazy as L
-- aeson:
import Data.Aeson (Value, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
-- optparse-applicative:
import Options.Applicative
-- partly:
import Partly.Json
import System.Disk.Partitions.MBR

-- | An optional argument option.
maybeOption :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
maybeOption m = nullOption 
  ( reader (Just . Just)
  & value Nothing
  & m )

-- | Options for displaying JSON.
data JsonOptions = JsonOptions
  { ugly              :: Bool
  , includeBootloader :: Bool }
  deriving (Eq, Show)

-- | Parse those options for displaying JSON.
parseJsonOptions :: Parser JsonOptions
parseJsonOptions = JsonOptions
  <$> switch
    ( long "ugly"
    & short 'u'
    & help "Don't prettify the JSON before writing it." )
  <*> switch
    ( long "include-bootloader"
    & short 'l'
    & help "Include the bootloader, base64-encoded." )

displayJson :: JsonOptions -> BootRecord -> L.ByteString
displayJson o = encoder . change
  where
    change = bootRecordToJson $ includeBootloader o
    encoder = if ugly o then encode else encodePretty

-- | Some options related to how we output things.
data Output = Output
  { outFile :: Maybe FilePath }
  deriving (Eq, Show)

-- | Parse those options.
parseOutput :: Parser Output
parseOutput = Output
  <$> maybeOption
    ( long "output"
    & short 'o'
    & metavar "file"
    & help "A file to write to; defaults to stdout." )

-- | Output a bytestring or a string, given some 'Output'.
output :: Output -> Either String L.ByteString -> IO ()
output = maybe (putStrLn <|> L.putStr)
  (\p -> writeFile p <|> L.writeFile p) . outFile 
  where (f <|> _) (Left a) = f a; (_ <|> f) (Right b) = f b
