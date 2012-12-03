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
    [ "bootSignature" .= object
      [ "asNum" .= bootSig b
      , "asHex" .= (printf "0x%04x" (bootSig b) :: String) ]
    , "partitions" .= partitions b ]
    -- TODO: should I include this here? idk.
    -- , "bootloader" .= bootloader b

instance FromJSON CHS where
  parseJSON (Object v) = CHS
    <$> v .: "head" <*> v .: "cylinder" <*> v .: "sector"

instance FromJSON PartitionEntry where
  parseJSON (Object v) = PartitionEntry
    <$> (v .: "status" >>= getStatus)
    <*> (v .: "chsFirst" >>= parseJSON)
    <*> (v .: "partitionType" >>= (.: "asNum"))
    <*> (v .: "chsLast" >>= parseJSON)
    <*>  v .: "lbaFirst"
    <*>  v .: "sectorCount"
    where
      -- Check that "status" is consistent.
      getStatus (Object v) = do
        asNum <- v .:? "asNum"
        chars <- v .:? "asHex"
        final <- case (asNum, chars) of
          (Nothing, Nothing) -> pure Nothing
          (Nothing, Just s) -> pure (readMaybe s)
          (Just x, Nothing) -> pure (Just x)
          (Just x, Just s) -> if readMaybe s == Just x
            then pure (Just x) else empty
        boots <- v .:? "bootable"
        case (boots, final) of
          (Just False, Nothing) -> pure 0x00
          (Just True, Nothing) -> pure 0x80 
          (Just False, Just v) -> if v `shiftR` 7 == 1
            then empty else pure v
          (Just True, Just v) -> if v `shiftR` 7 == 1
            then pure v else empty
          (Nothing, Nothing) -> pure 0x80
          (Nothing, Just v) -> pure v
      -- Read a value, if possible.
      readMaybe :: Read a => String -> Maybe a
      readMaybe x = case reads x of [(v, "")] -> Just v; _ -> Nothing;
