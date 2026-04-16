{- |
Module      : Data.UUID.V6
Copyright   : © 2025 ARJANEN Loïc Jean David

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable

RFC 9562 Version 6 UUID state machine.

The generated UUID is based on the hardware MAC
address and the system clock.

If we cannot lookup the MAC address we seed the
generator with a pseudo-random number.
-}

module Data.UUID.V6(nextUUID)
where

import Data.Bits
import Data.Maybe
import Data.Word

import Network.Info

import Data.UUID.Time
import Data.UUID.Types.Internal

-- | Returns a new UUID derived from the local hardware MAC
-- address and the current system time.
-- Is generated according to the Version 6 UUID specified in
-- RFC 9562.
--
-- Returns 'Nothing' if you request UUIDs too quickly.
nextUUID :: IO (Maybe UUID)
nextUUID = do
  res <- stepTime
  case res of
    Just (mac', c, t) -> return $ Just $ makeUUID t c mac'
    _ -> return Nothing

makeUUID :: Word64 -> Word16 -> MAC -> UUID
makeUUID time clock mac' =
    buildFromBytes 6 /-/ tHigh /-/ tMid /-/ tLow /-/ clock /-/ (MACSource mac')
    where tHigh = (fromIntegral (time `shiftR` 28)) :: Word32
          tMid = (fromIntegral (time `shiftR` 12)) :: Word16
          tLow = (fromIntegral (time .&. 0xFFF)) :: Word16
