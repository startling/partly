{-# Language OverloadedStrings #-}
module Partly.Json where
-- base:
import Control.Applicative
import Data.Bits
import Data.Maybe
import Data.Word
import Text.Printf
-- bytestring:
import qualified Data.ByteString as B
-- vector:
import Data.Vector ((!?))
-- aeson:
import Data.Aeson
import Data.Aeson.Types
-- base64-bytestring:
import qualified Data.ByteString.Base64 as Base64
-- partly:
import System.Disk.Partitions.MBR

instance ToJSON CHS where
  toJSON (CHS h s c) = object
    [ "cylinder" .= c
    , "head" .= h
    , "sector" .= s ]

instance ToJSON PartitionEntry where
  toJSON p = object
    [ "chsFirst" .= chsFirst p
    , "chsLast" .= chsLast p
    , "lbaFirst" .= lbaFirst p
    , "sectorCount" .= sectors p
    -- TODO: how much data am I losing here?
    , "bootable" .= bootable p
    -- TODO: give the type of the partition, if we know it.
    , "partitionType" .= partitionType p ]

instance ToJSON PartitionTable where
  toJSON (PartitionTable _1 _2 _3 _4) = toJSON [_1, _2, _3, _4]

instance ToJSON BootRecord where
  toJSON = bootRecordToJson False

-- | Write a boot record to JSON, given whether to include the
-- base64-encoded bootloader.
bootRecordToJson :: Bool -> BootRecord -> Value
bootRecordToJson i b = object $
  [ "bootSignature" .= (bootSig b == 0xaa55)
  , "partitions" .= partitions b ]
  ++ if i then [ "bootloader" .= Base64.encode (bootloader b) ] else []

instance FromJSON CHS where
  parseJSON (Object v) = CHS
    <$> v .: "head" <*> v .: "cylinder" <*> v .: "sector"

instance FromJSON PartitionEntry where
  parseJSON (Object v) = PartitionEntry
    <$> (v .: "status" >>= \x -> case x of
        (Bool b) -> return $ if b then 0x80 else 0x00
        (Number n) -> parseJSON $ Number n)
    <*> (v .: "chsFirst" >>= parseJSON)
    <*> (v .: "partitionType")
    <*> (v .: "chsLast" >>= parseJSON)
    <*>  v .: "lbaFirst"
    <*>  v .: "sectorCount"

instance FromJSON PartitionTable where
  parseJSON (Array a) = PartitionTable
    <$> getNth 0
    <*> getNth 1
    <*> getNth 2
    <*> getNth 3
    where getNth = maybe (pure nullPartition) parseJSON . (!?) a

instance FromJSON BootRecord where
  parseJSON (Object v) = do
    sig <- v .:? "bootSignature" >>= \x -> case x of
      Nothing -> pure 0xaa55
      (Just (Bool True)) -> pure 0xaa55
      (Just (Bool False)) -> pure 0xaa55
      (Just n) -> parseJSON n
    ptt <- v .: "partitions" >>= parseJSON
    btl <- v .: "bootloader" <&> maybe emptyBootloader Base64.decodeLenient
    return $ BootRecord btl ptt sig
    where
      (<&>) = flip fmap
      if' x y b = if b then x else y

-- | The empty bootloader -- 446 empty bytes.
emptyBootloader :: B.ByteString
emptyBootloader = B.replicate 446 0

-- TODO: allow filepaths for bootloaders?
-- TODO: ToJSON instances should use numbers for bootSignature and status
--   when they're nonstandard; FromJSON instances should respect these.
