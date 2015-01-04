
-- | Internal utilites
module Data.Word.Util where

import Data.Bits
import Data.Word

w8to32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w8to32 w0s w1s w2s w3s =
    (w0 `shiftL` 24) .|. (w1 `shiftL` 16) .|. (w2 `shiftL` 8) .|. w3
  where
    w0 = fromIntegral w0s
    w1 = fromIntegral w1s
    w2 = fromIntegral w2s
    w3 = fromIntegral w3s

w8to16 :: Word8 -> Word8 -> Word16
w8to16 w0s w1s =
    (w0 `shiftL` 8) .|. w1
  where
    w0 = fromIntegral w0s
    w1 = fromIntegral w1s

w16to32 :: Word16 -> Word16 -> Word32
w16to32 w0s w1s =
    (w0 `shiftL` 16) .|. w1
  where
    w0 = fromIntegral w0s
    w1 = fromIntegral w1s

w32to64 :: Word32 -> Word32 -> Word64
w32to64 w0s w1s =
    (w0 `shiftL` 32) .|. w1
  where
    w0 = fromIntegral w0s
    w1 = fromIntegral w1s
