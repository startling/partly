{-# Language FlexibleInstances #-}
module Partly.Common where
-- base:
import Control.Applicative
import Data.List
-- bytestring:
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
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

class Outputs o where
  toFile :: FilePath -> o -> IO ()
  toScreen :: o -> IO ()

instance Outputs String where
  toFile = writeFile
  toScreen = putStrLn

instance Outputs B.ByteString where
  toFile = B.writeFile
  toScreen = B.putStr

instance Outputs L.ByteString where
  toFile = L.writeFile
  toScreen = L.putStr

-- | Output a bytestring or a string, given some 'Output'.
output :: Outputs o => Output -> o -> IO ()
output = maybe toScreen toFile . outFile

-- | Some options for displaying a thing conditionally as JSON.
data Display = Display
  { useJson :: Maybe JsonOptions
  , outputs :: Output }
  deriving (Eq, Show)

-- | Parse the options for displaying things.
parseDisplay :: Parser Display
parseDisplay = Display 
  <$> optional (json *> parseJsonOptions)
  <*> parseOutput
  where
    json :: Parser Bool
    json = flag' True
      ( long "json"
      & short 'j'
      & help "Output as JSON." )

-- | Display a 'BootRecord', potentially as JSON.
display :: Display -> BootRecord -> IO ()
display d = output (outputs d) . fn 
  where fn = maybe (runPut . put) displayJson $ useJson d

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
