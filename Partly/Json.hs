{-# Language OverloadedStrings #-}
module Partly.Json where
-- base:
import Control.Applicative
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
    , "status" .= object
      [ "bootable" .= bootable p
      , "asNum" .= status p
      , "asHex" .= (printf "0x%02x" (status p) :: String) ]
    , "partitionType" .= object
      [ "asNum" .= partitionType p ]]
      -- TODO: print the type of partition

instance ToJSON PartitionTable where
  toJSON (PartitionTable _1 _2 _3 _4) = toJSON [_1, _2, _3, _4]

instance ToJSON BootRecord where
  toJSON b = object
    [ "bootSignature" .= object
      [ "asNum" .= bootSig b
      , "asHex" .= (printf "0x%04x" (bootSig b) :: String) ]
    , "partitions" .= partitions b ]
    -- TODO: should I include this here? idk.
    -- , "bootloader" .= bootloader b

instance FromJSON CHS where
  parseJSON (Object v) = CHS
      <$> v .: "head" <*> v .: "cylinder" <*> v .: "sector"

-- TODO: Figure out how to handle inconsistent data in partition table entries.
-- TODO: Write a way to turn some json into a partitionTableEntry, keeping in mind
--   that a user may supply too many or inconsistent data.
-- TODO: Apply that same thing in BootRecord, keeping in mind inconsistent data in
--   the boot signature.
