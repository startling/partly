{-# Language OverloadedStrings #-}
module Partly.Json where
-- base:
import Control.Applicative
import Data.Bits
import Data.Maybe
import Data.Word
import Text.Printf
-- aeson:
import Data.Aeson
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
