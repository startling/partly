module Partly.Common where
-- base:
import Control.Applicative
import Data.List
-- bytestring:
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary (get)
import Data.Binary.Get
-- aeson:
import Data.Aeson (Value, encode, decode)
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
output :: Output -> L.ByteString -> IO ()
output = maybe L.putStr L.writeFile . outFile

-- | Output a string, given some 'Output'.
outputEither :: Output -> Either String L.ByteString -> IO ()
outputEither = maybe (putStrLn <!> L.putStr)
  (\p -> writeFile p <!> L.writeFile p) . outFile
  where (f <!> g) e = case e of Left a -> f a; Right b -> g b

-- | Some options related to how we get input.
data Input = Input
  { inFile   :: FilePath
  -- | Whether this should be read as JSON; 'Nothing' here means to infer
  -- from the extension type.
  , fromJson :: Maybe Bool }
  deriving (Eq, Show)

-- | Parse those input options, given a string to use for
-- the description of the the input file.
parseInput :: String -> Parser Input
parseInput s = Input
  <$> argument str
    ( help s
    & metavar "file" )
  <*> flag Nothing (Just True)
    ( long "from-json"
    & short 'f'
    & help "Treat the input file as JSON." )

-- | Get a BootRecord as input.
input :: Input -> IO BootRecord
input i = reader <$> L.readFile (inFile i)
  where
    isJson = maybe (".json" `elem` tails (inFile i)) id $ fromJson i
    reader = if isJson then
      maybe (error "Problems reading JSON") id . decode else
      runGet (get :: Get BootRecord)
