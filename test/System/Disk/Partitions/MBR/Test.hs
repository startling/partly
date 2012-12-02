module System.Disk.Partitions.MBR.Test where
-- base:
import Control.Applicative
-- bytestring:
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
-- quickcheck:
import Test.QuickCheck
-- partly:
import System.Disk.Partitions.MBR

-- | Generate a random lazy ByteString of some N bytes.
genBytes :: Int -> Gen L.ByteString
genBytes = fmap L.pack . flip vectorOf arbitrary

-- | Test whether a Get succeeds for any bytestring of some n bytes.
succeeds :: Int -> Get a -> Gen Bool
succeeds n get = isRight . runGetOrFail get <$> genBytes n
  where isRight (Right _) = True; isRight _ = False;

-- | Test whether a getter and a putter are the exact inverses for
-- bytestrings of some length.
bijective :: Int -> Get b -> (b -> Put) -> Gen Bool
bijective n get put = (`fmap` genBytes n) $ \bs ->
  bs == (runPut . put . runGet get $ bs)

-- | Test that getting always succeeds.
successes :: [Gen Bool]
successes = 
  -- We should be able to get a CHS out of any three bytes,
  [ succeeds   3 (get :: Get CHS)
  -- a partition table entry out of any sixteen,
  , succeeds  16 (get :: Get PartitionEntry)
  -- a partition table out of any sixty-four,
  , succeeds  64 (get :: Get PartitionTable)
  -- and an MBR out of any 512.
  , succeeds 512 (get :: Get BootRecord) ]

-- | Test that getting and putting form an isomorphism.
bijections :: [Gen Bool]
bijections =
  [ bijective   3 get (put :: CHS -> Put)
  , bijective  16 get (put :: PartitionEntry -> Put)
  , bijective  64 get (put :: PartitionTable -> Put)
  , bijective 512 get (put :: BootRecord -> Put) ]
