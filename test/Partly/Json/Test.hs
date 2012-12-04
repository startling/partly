module Partly.Json.Test where
-- aeson
import Data.Aeson
-- QuickCheck
import Test.QuickCheck
-- partly
import System.Disk.Partitions.MBR
import System.Disk.Partitions.MBR.Gen
import Partly.Json

-- | Test that we can serialize and deserialize a thing
-- to and from JSON without losing information.
iso :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
iso c = decode (encode c) == Just c
