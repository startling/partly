module Partly.Make where
-- base:
import Data.List
import Data.Char
import Data.Word
import Control.Applicative
-- bytestring:
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary (put, get)
import Data.Binary.Get
import Data.Binary.Put
-- aeson:
import Data.Aeson (encode, decode)
import Data.Aeson (fromJSON, Value)
import Data.Aeson.Types (parseEither, Result)
import Data.Aeson.Encode.Pretty
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json

data MakeOptions = MakeOptions
  { from    :: Maybe FilePath
  , json    :: Bool
  , ugly    :: Bool
  , include :: Bool
  , output  :: Maybe FilePath
  , change  :: Delta }
  deriving (Eq, Show)

data Delta = Delta
  { btl :: Maybe FilePath
  , sig :: Maybe Word16 }
  deriving (Eq, Show)

makeOptions :: Parser MakeOptions
makeOptions = MakeOptions
  <$> maybeOption
    ( long "from"
    & short 'f'
    & metavar "file"
    & help "A bootloader to base this one on, either binary or JSON." )
  <*> switch
    ( long "json"
    & short 'j'
    & help "Output the MBR as JSON rather than a binary blob." )
  <*> switch
    ( long "ugly"
    & short 'u'
    & help "Don't prettify the JSON before writing it." )
  <*> switch
    ( long "include-bootloader"
    & short 'l'
    & help "Include the bootloader, base64-encoded, in JSON output." )
  <*> maybeOption
    ( long "output"
    & short 'o'
    & metavar "file"
    & help "Write to a file rather than stdout." )
  -- TODO: should "output" be required, so that "partly make" gives you
  -- a help screen?
  <*> (Delta
    <$> maybeOption
      ( long "bootloader"
      & short 'b'
      & metavar "file"
      & help "A file to include as a bootloader." )
    <*> (fmap . (=<<)) getSignature (maybeOption
      ( long "signature"
      & short 's'
      & metavar "sig"
      & help "Set the boot signature to some uint16; also accepts 't' and 'f'.")))
  where
    maybeOption :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
    maybeOption m = nullOption $ reader (Just . Just) & m & value Nothing
    getSignature :: String -> Maybe Word16
    getSignature s = case toLower <$> s of
      "t" -> Just 0xaa55; "true" -> Just 0xaa55;
      "f" -> Just 0x0000; "false" -> Just 0x0000;
      _ -> case reads s of [(v, "")] -> Just v; _ -> Nothing;

makeParser :: ParserInfo MakeOptions
makeParser = info makeOptions
  ( progDesc "Create an MBR, potentially based on some existing one."
  & fullDesc)

applyDelta :: Delta -> BootRecord -> IO BootRecord
applyDelta d b = do
  b <- apply b (btl d) $ \p -> (`fmap` B.readFile p)
    $ \x -> b { bootloader = x}
  b <- apply b (sig d) $ \x -> return b { bootSig = x }
  return b
  where apply b v fn = maybe (return b) fn v

make :: MakeOptions -> IO ()
make m = do
  base <- case from m of
    Nothing -> return nullBootRecord
    (Just f) -> case ".json" `elem` tails f of
       True -> L.readFile f >>= decodeJson
       False -> fmap (runGet get) $ L.readFile f
  new <- writer <$> applyDelta (change m) base
  maybe (L.putStr new) (flip L.writeFile new) $ output m
  return ()
  where
    decodeJson = maybe (fail "problems reading JSON") return . decode
    writer = case (json m, ugly m) of
      (False, _) -> runPut . put
      (True, True) -> encode . bootRecordToJson (include m)
      (True, False) -> encodePretty . bootRecordToJson (include m)

