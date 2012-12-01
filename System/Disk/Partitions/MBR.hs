-- | Types for dealing with the old-fasioned and modern Master Boot Records.
-- This does not cover things like the GUID partition table or any of the
-- weird variations like AAP or NEWLDR.
module System.Disk.Partitions.MBR where
-- base:
import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
-- bytestring:
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- binary:
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- | The so-called mystery bytes on Windows 95B, 98, 98SE, and Me --
-- in fact, they're a timestamp and a drive number. 
-- See http://thestarman.pcministry.com/asm/mbr/mystery.htm .
data Timestamp = Timestamp
  { physicalDrive :: Word8
  , seconds       :: Word8
  , minutes       :: Word8
  , hours         :: Word8 }
  deriving (Eq, Show)

instance Binary Timestamp where
  get = Timestamp <$> get <*> get <*> get <*> get
  put = (sequence_ .) . sequence $ [put . physicalDrive
    , put . seconds, put . minutes, put . hours]

-- | A representation of the cylinder/head/sector address in MBRs.
data CHS = CHS
  -- | The head number.
  { head     :: Word8
  -- | The sector number; this is actually a six-bit number, but
  -- Haskell doesn't have a convenient way to deal with those.
  , sector   :: Word8
  -- | The cylinder number; likewise, this is actually a 10-bit number.
  , cylinder :: Word16 }
  deriving (Eq, Show)

instance Binary CHS where
  get = do
    (h, s, c) <- (,,) <$> getWord8 <*> getWord8 <*> (fromIntegral <$> getWord8)
    return . CHS h ((s << 2) >> 2) $ ((fromIntegral s) << 2) .&. fromIntegral c
    where (<<) = shiftL; (>>) = shiftR;
  put (CHS h s c) = do
    putWord8 h
    -- Mask away the high two bits of s and use the high two bits of c.
    putWord8 $ s .&. 0x3f .|. ((fromIntegral c >> 2) .&. 0xc0)
    -- Mask away the high byte of c.
    putWord8 . fromIntegral $ 0x00ff .&. c
    where (>>) = shiftR
    
-- | Partition entries themselves are somewhat intricate.
data PartitionEntry = PartitionEntry
  -- | A bitfield describing this partition. An 0x00 here means it's inactive;
  -- having bit 7 set (i.e. > 0x80) means bootable; anything less is invalid.
  { status        :: Word8
  -- | The CHS address of the first absolute sector of the partition.
  , chsFirst      :: CHS
  -- | A partition type; for specifics, see the following document:
  -- http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
  , partitionType :: Word8
  -- | The CHS address of the last absolute sector of the partition.
  , chsLast       :: CHS
  -- | The logical block address of the first absolute sector.
  , lbaFirst      :: Word32
  -- | The number of sectors in the partition.
  , sectors       :: Word32 }
  deriving (Eq, Show)

instance Binary PartitionEntry where
  get = PartitionEntry <$> get <*> get <*> get <*> get
    <*> getWord32le <*> getWord32le
  put = (sequence_ .) . sequence $ [put . status, put . chsFirst
    , put . partitionType, put . chsLast
    , putWord32le . lbaFirst, putWord32le . sectors]

-- | An MBR partition table consists of (up to?) four partition entries.
data PartitionTable = PartitionTable
  { first, second, third, fourth :: PartitionEntry }
  deriving (Eq, Show)

instance Binary PartitionTable where
  get = PartitionTable <$> get <*> get <*> get <*> get
  put = (sequence_ .) . sequence $ [put . first, put . second
    , put . third, put . fourth]

-- | The structure of a Master Boot Record is as follows...
data BootRecord = BootRecord
  -- | The first piece of data on a disk with a Master Boot Record is some
  -- bootloader code that gets loaded to address 0x7c00 in memory. N.B:
  -- this may include the data for the Windows timestamp, if it exists. It
  -- will also include the optional disk signature, if it exists -- thus this
  -- field is always 446 bytes long.
  { bootloader :: ByteString
  -- | Four partition table entries.
  , partitions :: PartitionTable
  -- | Finally, the boot signature. 
  , bootSig    :: Word16 }
  deriving (Eq, Show)

instance Binary BootRecord where
  get = BootRecord <$> getByteString 446 <*> get <*> getWord16le
  put = (sequence_ .) . sequence $ [ putByteString . B.take 446 . bootloader
    , put . partitions , putWord16le . bootSig ]

-- | Get the completely-optional, obsolete disk timestamp used by some old
-- versions of Windows.
getTimestamp :: BootRecord -> Maybe Timestamp
getTimestamp (BootRecord b _ _) = do
  -- Check that it's long enough.
  guard $ B.length b > 0x0df
  -- Check that it has the two zero bytes at 0x0da.
  guard $ B.index b 0x0da == 0 && B.index b 0x0db == 0
  -- Get the four bytes at 0x0dc.
  let _1 : _2 : _3 : _4 : _ = B.unpack . B.take 4 . B.drop 0x0dc $ b
  return $ Timestamp _1 _2 _3 _4

-- | Get the optional disk signature from a Bootrecord's bootloader.
getDiskSignature :: BootRecord -> Maybe Word32
getDiskSignature (BootRecord b _ _) = do
  -- check that it's long enough.
  guard $ B.length b > 0x1bd
  -- Check that it has the two zero bytes at 0x1bc
  guard $ B.index b 0x1bc == 0 && B.index b 0x1bd == 0
  -- Get the four bytes at 0x1b8...
  let [_1, _2, _3, _4] = map fromIntegral . B.unpack . B.take 4. B.drop 0x1b8 $ b
  -- And construct a word32 out of them, little-endian style.
  return $ (_4 << 24) .|. (_3 << 16) .|. (_2 << 8) .|. _1
  where (<<) = shiftL
