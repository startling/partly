module System.Disk.Partitions.MBR.Gen where
-- base
import Control.Applicative
-- bytestring:
import qualified Data.ByteString as B
-- QuickCheck
import Test.QuickCheck
-- partly
import System.Disk.Partitions.MBR

instance Arbitrary CHS where
  arbitrary = CHS <$> arbitrary <*> _6bit <*> _10bit
    where
      _6bit = choose (0x00, 0x3f)
      _10bit = choose (0x00, 0x3ff)

instance Arbitrary PartitionEntry where
  arbitrary = PartitionEntry <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PartitionTable where
  arbitrary = PartitionTable <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary BootRecord where
  arbitrary = BootRecord <$> genBootloader <*> arbitrary <*> arbitrary

-- | Generate a bytestring 446 bytes long.
genBootloader :: Gen B.ByteString
genBootloader = B.pack <$> vectorOf 446 arbitrary
