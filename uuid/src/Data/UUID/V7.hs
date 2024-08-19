-- |
-- Module      : Data.UUID.V7
-- Copyright   : (c) Taylor Fausak
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
--
--
-- This module implements Version 7 UUIDs as specified
-- in RFC 9562 accessible here: <https://datatracker.ietf.org/doc/html/rfc9562>.
--
-- These UUIDs are partly time-ordered, which improves database locality.

module Data.UUID.V7 ( generate, build ) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Time.Clock.System as Time
import qualified Data.UUID.Types as UUID
import qualified Data.Word as Word
import qualified System.Entropy as Entropy

-- | Generates a 'UUID' using the current time (from 'Time.getSystemTime') and
-- random data (from 'Entropy.getEntropy').
--
-- @since 1.3.17
generate :: IO UUID.UUID
generate = do
  t <- Time.getSystemTime
  -- Note that we only need 74 bits (12 + 62) of randomness. That's a little
  -- more than 9 bytes (72 bits), so we have to request 10 bytes (80 bits) of
  -- entropy. The extra 6 bits are discarded.
  b <- Entropy.getEntropy 10
  pure $
    let u8_u64 = fromIntegral :: Word.Word8 -> Word.Word64
        f = Bits.shift . u8_u64 . ByteString.index b
        r = f 0 0 + f 1 8
        s = f 2 0 + f 3 8 + f 4 16 + f 5 24 + f 6 32 + f 7 40 + f 8 48 + f 9 56
     in build t r s

-- | Builds a 'UUID' using the provided fields. Typically you will want to use
-- the 'generate' function instead.
--
-- @since 1.3.17
build ::
  -- | Corresponds to the @unix_ts_ms@ field.
  Time.SystemTime ->
  -- | Corresponds to the @rand_a@ field. Only the low 12 bits are used.
  Word.Word64 ->
  -- | Corresponds to the @rand_b@ field. Only the low 62 bits are used.
  Word.Word64 ->
  UUID.UUID
build t r s =
  let i64_u64 = fromIntegral :: Int.Int64 -> Word.Word64
      u32_u64 = fromIntegral :: Word.Word32 -> Word.Word64
      unix_ts_ms =
        Bits.shift
          ( (i64_u64 (Time.systemSeconds t) * 1000)
              + u32_u64 (div (Time.systemNanoseconds t) 1000000)
          )
          16
      ver = Bits.shift 0x7 12 :: Word.Word64
      rand_a = r Bits..&. 0x0fff -- 0x0fff = 2^12 - 1
      var = Bits.shift 0x2 62 :: Word.Word64
      rand_b = s Bits..&. 0x3fffffffffffffff -- 0x3fffffffffffffff = 2^62 - 1
   in UUID.fromWords64 (unix_ts_ms + ver + rand_a) (var + rand_b)