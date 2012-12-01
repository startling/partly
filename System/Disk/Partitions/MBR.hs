-- | Types for dealing with the old-fasioned and modern Master Boot Records.
-- This does not cover things like the GUID partition table or any of the
-- weird variations like AAP or NEWLDR.
module System.Disk.Partitions.MBR where
-- base:
import Data.Word
import Data.ByteString

-- | The so-called mystery bytes on Windows 95B, 98, 98SE, and Me --
-- in fact, they're a timestamp and a drive number. 
-- See http://thestarman.pcministry.com/asm/mbr/mystery.htm .
data Timestamp = Timestamp
  { physicalDrive :: Word8
  , seconds       :: Word8
  , minutes       :: Word8
  , hours         :: Word8 }
  deriving (Eq, Show)

-- | Partition entries themselves are somewhat intricate.
data PartitionEntry = PartitionEntry
  -- | A bitfield describing this partition. An 0x00 here means it's inactive;
  -- having bit 7 set (i.e. > 0x80) means bootable; anything less is invalid.
  { status        :: Word8
  -- | The CHS address of the first absolute sector of the partition.
  , chsFirst      :: (Word8, Word8, Word8)
  -- | A partition type; for specifics, see the following document:
  -- http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
  , partitionType :: Word8
  -- | The CHS address of the last absolute sector of the partition.
  , chsLast       :: (Word8, Word8, Word8)
  -- | The logical block address of the first absolute sector.
  , lbaFirst      :: Word32
  -- | The number of sectors in the partition.
  , sectors       :: Word32 }
  deriving (Eq, Show)

-- | An MBR partition table consists of (up to?) five partition entries.
data PartitionTable = PartitionTable
  { first, second, third, fourth, fifth :: PartitionEntry }
  deriving (Eq, Show)

-- | The structure of a Master Boot Record is as follows, with some optional
-- fields for optional data.
data BootRecord = BootRecord
  -- | The first piece of data on a disk with a Master Boot Record is some
  -- bootloader code that gets loaded to address 0x7c00 in memory. N.B:
  -- this may include the data for the Windows timestamp, if it exists. It
  -- will also include the optional disk signature, if it exists -- thus this
  -- field is always 446 bytes long.
  { bootloader :: ByteString
  -- | The completely-optional obsolete disk timestamp use by some old versions
  -- of Windows.
  , timestamp  :: Maybe Timestamp
  -- | The optional disk signature.
  , diskSig    :: Maybe Word32
  -- | Five partition table entries.
  , partitions :: PartitionTable
  -- | Finally, the boot signature. 
  , bootSig    :: Word16 }
  deriving (Eq, Show)
