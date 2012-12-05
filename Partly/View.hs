module Partly.View where
-- base:
import Control.Applicative
import Text.Printf
-- bytestring:
import qualified Data.ByteString as B
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json
import Partly.Common

viewJsonOptions :: Parser (JsonOptions, Input, Output)
viewJsonOptions = (,,)
  <$> parseJsonOptions
  <*> parseInput "The file to parse and inspect."
  <*> parseOutput

viewJson :: (JsonOptions, Input, Output) -> IO ()
viewJson (j, i, o) = input i >>= output o . displayJson j

-- | A wrapper for a labelled, displayable field of a boot record.
data Field = (BootRecord -> Either String B.ByteString) :?: String

instance Show Field where
  show (_ :?: s) = printf "'%s'" s

-- | We can turn a labelled field of a boot record into a command.
fieldCommand :: Field -> InfoMod ViewCommand
  -> Mod CommandFields ViewCommand
fieldCommand f@(fn :?: s) m = command s . flip info m $
  ViewField f
    <$> parseInput "The file to parse and inspect"
    <*> parseOutput

infixr 8 :?:

data ViewCommand
  = ViewJson (JsonOptions, Input, Output)
  | ViewField Field Input Output
  deriving (Show)

viewParser :: ParserInfo ViewCommand
viewParser = info
  ( subparser
    ( command "json"
      ( info (ViewJson <$> viewJsonOptions )
        ( progDesc "Read a boot record into JSON." ))
    & fieldCommand (sigFn :?: "signature")
      ( progDesc "View the boot signature of an MBR." )
    & fieldCommand (Right . bootloader :?: "bootloader")
      ( progDesc "View the bootloader of an MBR." )))
  ( progDesc "Inspect a boot record."
  & fullDesc )
  where
    sigFn = Left . printf "%04x" . bootSig

view :: ViewCommand -> IO ()
view c = case c of
  ViewJson vj -> viewJson vj
  ViewField (fn :?: _) i o -> input i >>= outputEither o . fn
