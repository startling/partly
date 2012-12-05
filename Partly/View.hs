module Partly.View where
-- base:
import Control.Applicative
-- bytestring:
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as L
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
viewJson (j, i, o) = do
  mbr <- input i
  output o . displayJson j $ mbr

data ViewCommand
  = ViewJson (JsonOptions, Input, Output)
  deriving (Eq, Show)

viewParser :: ParserInfo ViewCommand
viewParser = info
  ( subparser
    ( command "json"
      ( info (ViewJson <$> viewJsonOptions )
        ( progDesc "Read a boot record into JSON." ))))
  ( progDesc "Inspect a boot record."
  & fullDesc )

view :: ViewCommand -> IO ()
view c = case c of ViewJson vj -> viewJson vj;
