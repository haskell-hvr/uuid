
-- | Internal utilites
module Data.Word.Util where

import Data.Bits
import Data.Word

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
