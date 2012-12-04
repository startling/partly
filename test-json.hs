module Main where
-- base
import Control.Monad
import Control.Applicative
import System.Exit
-- bytestring
import qualified Data.ByteString.Lazy as L
-- aeson
import Data.Aeson (Value, decode, FromJSON, encode)
-- QuickCheck
import Test.QuickCheck
-- partly
import System.Disk.Partitions.MBR
import System.Disk.Partitions.MBR.Gen
import Partly.Json

-- | Test that we can serialize and deserialize a thing
-- to and from JSON without losing information.
iso :: (Eq a, FromJSON a) => (a -> L.ByteString) -> a -> Bool
iso e c = decode (e c) == Just c

testIso :: (Eq a, Show a, FromJSON a, Arbitrary a)
  => String -> (a -> L.ByteString) -> IO Bool
testIso s e = do
  putStrLn $ "--> " ++ s
  r <- quickCheckResult $ iso e
  return $ resultToBool r

resultToBool :: Result -> Bool
resultToBool (Success _ _ _) = True
resultToBool (GaveUp _ _ _) = False
resultToBool (Failure _ _ _ _ _ _ _) = False

main :: IO ()
main = do
  putStrLn "Testing whether 'encode' and 'fromJust . decode' form an isomorphism."
  is <- all id <$> sequence
    [ testIso "CHS" (encode :: CHS -> L.ByteString)
    , testIso "PartitionEntry" (encode :: PartitionEntry -> L.ByteString)
    , testIso "PartitionTable" (encode :: PartitionTable -> L.ByteString)
    , testIso "BootRecord" (encode . bootRecordToJson True) ]
  unless is exitFailure
