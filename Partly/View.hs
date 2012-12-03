module Partly.View where
-- base:
import Control.Applicative
import Control.Monad
import Text.Printf
-- bytestring:
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
-- optparse-applicative:
import Options.Applicative
-- partly:
import System.Disk.Partitions.MBR

data ViewBootRecord = ViewBootRecord
  { viewBootSector :: Bool
  , viewSignature  :: Bool
  , viewPartitions :: ViewPartitionTable
  , viewFile       :: FilePath }
  deriving (Eq, Show)

data ViewPartitionTable = ViewPartitionTable
  { viewFirst  :: Bool
  , viewSecond :: Bool
  , viewThird  :: Bool
  , viewFourth :: Bool }
  deriving (Eq, Show)

viewOptions :: Parser ViewBootRecord
viewOptions = ViewBootRecord
  <$> switch
      ( long "boot"
      & help "Whether to show the bootloader code." )
  <*> switch
      ( long "sig"
      & long "signature"
      & help "Whether to show the boot signature.")
  <*> (ViewPartitionTable
      <$> switch
          ( short '1'
          & help "Whether to show the first partition.")
      <*> switch
          ( short '2'
          & help "Whether to show the second partition.")
      <*> switch
          ( short '3'
          & help "Whether to show the third partition.")
      <*> switch
          ( short '4'
          & help "Whether to show the fourth partition."))
  <*> argument Just
      ( help "The file to parse and view."
      & metavar "file" )

view :: ViewBootRecord -> IO ()
view (ViewBootRecord sec sig (ViewPartitionTable _1 _2 _3 _4) f) = do
  l <- L.readFile f
  -- TODO: If all the flags are False, flip them all.
  let br = runGet get l
  when sec .
    putStrLn $ "bootloader: " ++ show (bootloader br)
  when sig . putStrLn .
    printf "signature: 0x%04x" $ bootSig br
  when (_1 || _2 || _3 || _4) . putStrLn $
    printf "some partitions"
