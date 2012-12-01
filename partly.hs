module Main where
-- base:
import Control.Applicative
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
      ( long "signature"
      & long "sig"
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

view :: Mod CommandFields ViewBootRecord
view = command "view" $ ParserInfo
  { infoParser      = viewOptions
  , infoFullDesc    = False
  , infoProgDesc    = "View the contents of an MBR."
  , infoHeader      = ""
  , infoFooter      = ""
  , infoFailureCode = 1 }

master :: ParserInfo ViewBootRecord
master = info (subparser view)
  ( fullDesc
  & progDesc "View, create, or alter a DOS-style master boot record.")

main :: IO ()
main = execParser master >>= print