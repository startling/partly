module Partly.View where
-- base:
import Control.Applicative
import Text.Printf
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json
import Partly.Common

-- | Parse the options we can use for "partly view json".
viewJsonOptions :: Parser (Input, Display)
viewJsonOptions = (,)
  <$> parseInput "The file to parse and inspect."
  <*> parseDisplay

-- | Turn those options into an IO action.
viewJson :: (Input, Display) -> IO ()
viewJson (i, d) = input i >>= display d

-- | We can turn a labelled field of a boot record into a command.
fieldCommand :: Outputs b => String -> (BootRecord -> b)
  -> InfoMod ViewCommand -> Mod CommandFields ViewCommand
fieldCommand s fn m = command s . flip info m $
  ViewField 
    <$> parseInput "The file to parse and inspect."
    <*> ((. fn) <$> output <$> parseOutput)

-- | The type that the main program will hand to us.
data ViewCommand
  = ViewJson (Input, Display)
  | ViewField Input (BootRecord -> IO ())

-- | Combine all the commands together.
viewParser :: ParserInfo ViewCommand
viewParser = info
  ( subparser
    ( command "json"
      ( info (ViewJson <$> viewJsonOptions )
        ( progDesc "Read a boot record into JSON." ))
    & fieldCommand "signature"
      ( printf "%04x" . bootSig :: BootRecord -> String )
      ( progDesc "View the boot signature of an MBR." )
    & fieldCommand "bootloader" bootloader
      ( progDesc "View the bootloader of an MBR." )))
  ( progDesc "Inspect a boot record."
  & fullDesc )

-- | Execute the "view" command.
view :: ViewCommand -> IO ()
view c = case c of
  ViewJson vj -> viewJson vj
  ViewField i fn -> input i >>= fn
