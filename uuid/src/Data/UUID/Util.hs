{-# LANGUAGE TypeFamilies #-}
module Data.UUID.Util (
    UnpackedUUID(..)
  , unpack, pack
  , version
  , extractMac
  , extractTime
  , setTime
  ) where

import Prelude hiding (null)
import Data.Word
import Data.Word.Util
import Data.Bits
import Data.UUID.Types.Internal
import Network.Info
import Data.Int (Int64)

version :: UUID -> Int
version uuid =
    fromEnum ((time_hi_and_version unpacked `shiftR` 12) .&. 0xF)
  where
    unpacked = unpack uuid

-- Note UUID time is in 10^-7 seconds.
setTime :: (Integral a, Bits a) => UUID -> a -> Maybe UUID
setTime uuid t =
  if version uuid == 1
  then Just $ pack $ (unpack uuid){time_low = new_low_bits, time_mid = new_mid_bits, time_hi_and_version = new_hi_and_version_bits}
  else Nothing
       where new_low_bits = fromIntegral $ t .&. 0xFFFFFFFF
             new_mid_bits = fromIntegral $ (t `shiftR` 32) .&. 0xFFFF
             new_hi_and_version_bits = fromIntegral $ 0x1000 .|. ((t `shiftR` 48) .&. 0x0FFF)

extractTime :: UUID -> Maybe Int64
extractTime uuid =
  if version uuid == 1
  then Just $ fromIntegral $ w32to64 (w16to32 (timeAndVersionToTime $ time_hi_and_version unpacked) $ time_mid unpacked) (time_low unpacked)
  else Nothing
  where
    unpacked = unpack uuid

timeAndVersionToTime :: Word16 -> Word16
timeAndVersionToTime tv = tv .&. 0x0FFF

extractMac :: UUID -> Maybe MAC
extractMac uuid =
  if version uuid == 1
  then Just $
       MAC (node_0 unpacked) (node_1 unpacked) (node_2 unpacked) (node_3 unpacked) (node_4 unpacked) (node_5 unpacked)
  else Nothing
  where
    unpacked = unpack uuid
