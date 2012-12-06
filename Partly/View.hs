module Partly.View where
-- base:
import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Text.Printf
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR
import Partly.Json
import Partly.Common

-- | Parse the options we can use for "partly view json".
viewJsonOptions :: Parser (Input, JsonOptions, Maybe FilePath)
viewJsonOptions = (,,)
  <$> parseInput "The file to parse and inspect."
  <*> parseJsonOptions
  <*> parseOutput

-- | Turn those options into an IO action.
viewJson :: (Input, JsonOptions, Maybe FilePath) -> IO ()
viewJson (i, j, o) = input i >>= output o . displayJson j

-- | We can turn a labelled field of a boot record into a command.
fieldCommand :: Outputs b => String -> (BootRecord -> b)
  -> InfoMod ViewCommand -> Mod CommandFields ViewCommand
fieldCommand s fn m = command s . flip info m $
  ViewField 
    <$> parseInput "The file to parse and inspect."
    <*> ((. fn) <$> output <$> parseOutput)

-- | A header for when we want to pretty-print a partition.
partitionLabels :: String
partitionLabels =
  "#: b st ty [  sc - sh - ss ] [  fc - fh - fs ] lbafirst  sectors"

-- | Pretty-print a partition.
viewPartition :: Int -> PartitionEntry -> String
viewPartition i p = printf
  "%01d: %s %2x %2x [ %3x - %2x - %2x ] [ %3x - %2x - %2x ] %8x %8x"
  i (if bootable p then "!" else " ")
  (status p) (partitionType p)
  (cylinder $ chsFirst p) (head $ chsFirst p) (sector $ chsFirst p) 
  (cylinder $ chsLast p) (head $ chsLast p) (sector $ chsLast p)
  (lbaFirst p) (sectors p)

-- | Pretty-print the partition table of a boot record.
viewPartitions :: BootRecord -> String
viewPartitions br = unlines
  [ partitionLabels
  , replicate (length partitionLabels) '-'
  , viewPartition 1 $ first pt
  , viewPartition 2 $ second pt
  , viewPartition 3 $ third pt
  , viewPartition 4 $ fourth pt ]
  where pt = partitions br

-- | The type that the main program will hand to us.
data ViewCommand
  = ViewJson (Input, JsonOptions, Maybe FilePath)
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
      ( progDesc "View the bootloader of an MBR." )
    & fieldCommand "partitions" viewPartitions
      ( progDesc "Pretty-print the partition table." )))
  ( progDesc "Inspect a boot record."
  & fullDesc )

-- | Execute the "view" command.
view :: ViewCommand -> IO ()
view c = case c of
  ViewJson vj -> viewJson vj
  ViewField i fn -> input i >>= fn
