module Partly.Make where
-- base:
import Data.List (tails)
import Data.Char (toLower)
import Data.Word (Word16)
import Control.Applicative
-- bytestring:
import qualified Data.ByteString as B
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json
import Partly.Common

-- | A change we can apply to a boot record.
data Delta = Delta
  { btl :: Maybe FilePath
  , sig :: Maybe Word16 }
  deriving (Eq, Show)

-- | Apply some changes to a boot record.
applyDelta :: Delta -> BootRecord -> IO BootRecord
applyDelta d b = do
  b <- apply b (btl d) $ \p -> (`fmap` B.readFile p)
    $ \x -> b { bootloader = x}
  b <- apply b (sig d) $ \x -> return b { bootSig = x }
  return b
  where apply b v fn = maybe (return b) fn v

-- | The kinds of options the main program will hand us.
data MakeOptions = MakeOptions
  { from        :: Maybe FilePath
  , change      :: Delta
  , displayOpts :: Display }
  deriving (Eq, Show)

-- | A parser for those options.
makeOptions :: Parser MakeOptions
makeOptions = MakeOptions
  <$> maybeOption
    ( long "from"
    & short 'f'
    & metavar "file"
    & help "A bootloader to base this one on, either binary or JSON." )
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
  <*> parseDisplay
  where
    getSignature :: String -> Maybe Word16
    getSignature s = case toLower <$> s of
      "t" -> Just 0xaa55; "true" -> Just 0xaa55;
      "f" -> Just 0x0000; "false" -> Just 0x0000;
      _ -> case reads s of [(v, "")] -> Just v; _ -> Nothing;

-- | A description of the parsing options.
makeParser :: ParserInfo MakeOptions
makeParser = info makeOptions
  ( progDesc "Create an MBR, potentially based on some existing one."
  & fullDesc)

-- | Run the "make" command.
make :: MakeOptions -> IO ()
make m = maybe (return nullBootRecord)
  (input . flip Input Nothing) (from m)
  >>= applyDelta (change m)
  >>= display (displayOpts m)
