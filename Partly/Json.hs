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
  toJSON b = object
    [ "bootSignature" .= (bootSig b == 0xaa55)
    , "partitions" .= partitions b ]

-- | Write a boot record to JSON with the bootloader base64-encoded.
bootrecordWithBootloader :: BootRecord -> Value
bootrecordWithBootloader b = object
  [ "bootSignature" .= (bootSig b == 0xaa55)
  , "partitions" .= partitions b
  , "bootloader" .= Base64.encode (bootloader b) ]

instance FromJSON CHS where
  parseJSON (Object v) = CHS
    <$> v .: "head" <*> v .: "cylinder" <*> v .: "sector"

instance FromJSON PartitionEntry where
  parseJSON (Object v) = PartitionEntry
    <$> (v .: "status" <&> \x -> if x then 0x80 else 0x00)
    <*> (v .: "chsFirst" >>= parseJSON)
    <*> (v .: "partitionType")
    <*> (v .: "chsLast" >>= parseJSON)
    <*>  v .: "lbaFirst"
    <*>  v .: "sectorCount"
    where (<&>) = flip fmap

instance FromJSON PartitionTable where
  parseJSON (Array a) = PartitionTable
    <$> getNth 0
    <*> getNth 1
    <*> getNth 2
    <*> getNth 3
    where getNth = maybe (pure nullPartition) parseJSON . (!?) a

-- | The empty bootloader -- 446 empty bytes.
emptyBootloader :: B.ByteString
emptyBootloader = B.replicate 446 0

-- | Get a boot record out of JSON.
jsonBootRecord :: Value -> Parser BootRecord
jsonBootRecord (Object v) = do
  sig <- v .:? "bootSignature" <&> maybe 0xaa55 (if' 0xaa55 0)
  ptt <- v .: "partitions" >>= parseJSON
  btl <- v .: "bootloader" <&> maybe emptyBootloader Base64.decodeLenient
  return $ BootRecord btl ptt sig
  where
    (<&>) = flip fmap
    if' x y b = if b then x else y
-- TODO: allow filepaths for bootloaders  
