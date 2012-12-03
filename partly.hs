module Main where
-- base:
import Control.Applicative
-- optparse-applicative:
import Options.Applicative
-- partly:
import Partly.View

data Command
  = View ViewBootRecord
  deriving (Eq, Show)

parser :: ParserInfo Command
parser = info
  ( subparser
    ( command "view"
      ( info (View <$> viewOptions)
        ( progDesc "View the contents of an MBR." ))))
   ( progDesc "Inspect, create, or alter DOS-style master boot records." )

main :: IO ()
main = execParser parser >>= apply
  where
    apply :: Command -> IO ()
    apply c = case c of
      View v -> view v
