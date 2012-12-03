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
    , "partitionType" .= object
      [ "asNum" .= partitionType p ]]
      -- TODO: print the type of partition

instance ToJSON PartitionTable where
  toJSON (PartitionTable _1 _2 _3 _4) = toJSON [_1, _2, _3, _4]

instance ToJSON BootRecord where
  toJSON b = object
    [ "bootSignature" .= (bootSig b == 0xaa55)
    , "partitions" .= partitions b ]
    -- TODO: should I include this here? idk.
    -- , "bootloader" .= bootloader b

instance FromJSON CHS where
  parseJSON (Object v) = CHS
    <$> v .: "head" <*> v .: "cylinder" <*> v .: "sector"

instance FromJSON PartitionEntry where
  parseJSON (Object v) = PartitionEntry
    <$> (v .: "status" <&> \x -> if x then 0x80 else 0x00)
    <*> (v .: "chsFirst" >>= parseJSON)
    <*> (v .: "partitionType" >>= (.: "asNum"))
    <*> (v .: "chsLast" >>= parseJSON)
    <*>  v .: "lbaFirst"
    <*>  v .: "sectorCount"
    where (<&>) = flip fmap
