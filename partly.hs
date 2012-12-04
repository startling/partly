module Main where
-- base:
import Control.Applicative
-- optparse-applicative:
import Options.Applicative
-- partly:
import Partly.Make
import Partly.View

data Command
  = View ViewCommand
  | Make MakeOptions
  deriving (Eq, Show)

parser :: ParserInfo Command
parser = info
  ( subparser
    ( command "view" (View <$> viewParser)
    & command "make" (Make <$> makeParser) ))
  ( progDesc "Inspect, create, or alter DOS-style master boot records." )

main :: IO ()
main = execParser parser >>= apply
  where
    apply :: Command -> IO ()
    apply c = case c of
      View v -> view v; Make m -> make m;
