{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.UUID.V1
-- Copyright   : (c) 2008 Jason Dusek
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- RFC 4122 Version 1 UUID state machine.
--
-- Currently this is more useful as a reference. For
-- true machine-unique UUID generation see the
-- system-uuid package.

module Data.UUID.V1(nextUUID, MAC(..))
where

import Data.Time

import Data.Bits
import Data.Word

import Data.IORef
import System.IO
import System.IO.Unsafe

import Data.UUID.Builder
import Data.UUID.Internal

-- | Returns a new UUID derived from the local hardware MAC
-- address and the current system time.
-- Is generated according to the Version 1 UUID sepcified in
-- RFC 4122.
--
-- Returns nothing if UUIDs are generated to quickly
nextUUID :: MAC -> IO (Maybe UUID)
nextUUID m = do
  res <- stepTime
  case res of
    Just (c, t) -> return $ Just $ makeUUID t c m
    _ -> return Nothing


makeUUID :: Word64 -> Word16 -> MAC -> UUID
makeUUID time clock mac =
    buildFromBytes 1 /-/ tLow /-/ tMid /-/ tHigh /-/ clock /-/ (MACSource mac)
    where tLow = (fromIntegral time) :: Word32
          tMid = (fromIntegral (time `shiftR` 32)) :: Word16
          tHigh = (fromIntegral (time `shiftR` 48)) :: Word16  

newtype MACSource = MACSource MAC
instance ByteSource MACSource where
    z /-/ (MACSource (MAC a b c d e f)) = z a b c d e f
type instance ByteSink MACSource g = Takes3Bytes (Takes3Bytes g)

-- | Machine address of computer generating UUIDs.
data MAC = MAC !Word8 !Word8 !Word8
               !Word8 !Word8 !Word8

-- |Approximates the clock algorithm in RFC 4122, section 4.2
-- Isn't system wide or thread safe, nor does it properly randomize
-- the clock value on initialization.
stepTime :: IO (Maybe (Word16, Word64))
stepTime = do
  State c0 h0 <- readIORef state
  h1 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  if h1 > h0
    then  do
      writeIORef state $ State c0 h1
      return $ Just (c0, h1)
    else  do
      let
        c1 = succ c0
      if c1 <= 0x3fff -- when clock is initially randomized,
                      -- then this test will need to change
        then  do
          writeIORef state $ State c1 h1
          return $ Just (c1, h1)
        else  do
          return Nothing


{-# NOINLINE state #-}
state :: IORef State
state = unsafePerformIO $ do
  h0 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  newIORef $ State 0 h0 -- the 0 should be a random number


data State = State
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word64
 deriving (Show)



hundredsOfNanosSinceGregorianReform :: UTCTime -> Word64
hundredsOfNanosSinceGregorianReform t = floor $ 10000000 * dt
 where
  gregorianReform = UTCTime (fromGregorian 1582 10 15) 0
  dt = t `diffUTCTime` gregorianReform



