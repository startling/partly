module Partly.Make where
-- base:
import Data.List
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
  { from   :: Maybe FilePath
  , btl    :: Maybe FilePath
  , json   :: Bool
  , ugly   :: Bool
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
  <*> switch
    ( long "ugly"
    & short 'u'
    & help "Don't prettify the JSON before writing it." )
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
make m = do
  base <- case from m of
    Nothing -> return nullBootRecord
    (Just f) -> case ".json" `elem` tails f of
       True -> L.readFile f >>= decodeJson
       False -> fmap (runGet get) $ L.readFile f
  new <- case btl m of
    Nothing -> return base
    Just n -> (`fmap` B.readFile n) $ \x -> base { bootloader = x }
  let l = getter new
  maybe (L.putStr l) (flip L.writeFile l) $ output m
  return ()
  where
    decodeJson = maybe (fail "problems reading JSON") return . decode
    getter = case (json m, ugly m) of
      (False, _) -> runPut . put
      (True, True) -> encode
      (True, False) -> encodePretty
