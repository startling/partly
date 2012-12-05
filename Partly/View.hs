{-# Language ExistentialQuantification #-}
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

-- | We can turn a labelled field of a boot record into a command.
fieldCommand :: Outputs b => (BootRecord -> b)
  -> String -> InfoMod ViewCommand -> Mod CommandFields ViewCommand
fieldCommand fn s m = command s . flip info m $
  ViewField 
    <$> parseInput "The file to parse and inspect."
    <*> ((. fn) <$> output <$> parseOutput)

data ViewCommand
  = ViewJson (JsonOptions, Input, Output)
  | ViewField Input (BootRecord -> IO ())

viewParser :: ParserInfo ViewCommand
viewParser = info
  ( subparser
    ( command "json"
      ( info (ViewJson <$> viewJsonOptions )
        ( progDesc "Read a boot record into JSON." ))
    & fieldCommand sigFn "signature"
      ( progDesc "View the boot signature of an MBR." )
    & fieldCommand bootloader "bootloader"
      ( progDesc "View the bootloader of an MBR." )))
  ( progDesc "Inspect a boot record."
  & fullDesc )
  where
    sigFn :: BootRecord -> String
    sigFn = printf "%04x" . bootSig

view :: ViewCommand -> IO ()
view c = case c of
  ViewJson vj -> viewJson vj
  ViewField i fn -> input i >>= fn
