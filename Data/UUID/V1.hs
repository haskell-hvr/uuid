{-# OPTIONS_GHC -fno-cse #-}

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

module Data.UUID.V1(nextUUID)
where

import Data.Time

import Data.Bits
import Data.Word
import Data.Binary

import Data.IORef
import System.IO
import System.IO.Unsafe

import System.Info.MAC
import Data.MAC

import Data.UUID.Internal

-- | Returns a new UUID derived from the local hardware MAC
-- address and the current system time.
-- Is generated according to the Version 1 UUID sepcified in
-- RFC 4122.
--
-- Returns nothing if the hardware MAC address could not
-- be discovered.
nextUUID :: IO (Maybe UUID)
nextUUID = do
  res <- stepTime
  mac <- mac
  case (res, mac) of
    (Just (c, t), Just (MAC a' b' c' d' e' f')) -> do
      let
        (tL, tM, tH) = word64ToTimePieces t
        (cL, cH) = word16ToClockSeqPieces c
      return $ Just $ UUID tL tM tH cH cL $ Node a' b' c' d' e' f'
    _ -> return Nothing


-- |The bit layout and version number here used are described in clause 13 of
--  ITU X.667, from September 2004.
word64ToTimePieces :: Word64 -> (Word32, Word16, Word16) 
word64ToTimePieces w = (lo, mi, hi) 
 where
  lo = fromIntegral $ (w)
  mi = fromIntegral $ (w `shiftR` 32)
  hi = fromIntegral $ (w `shiftR` 48 .&. 0x0fff .|. 0x1000)


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
      if c1 < 2^14
        then  do
          writeIORef state $ State c1 h1
          return $ Just (c1, h1)
        else  do
          return Nothing


{-# NOINLINE state #-}
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


-- |Per clause 13 of ITU X.667, from September 2004.
word16ToClockSeqPieces :: Word16 -> (Word8, Word8)
word16ToClockSeqPieces w = (lo, hi)
 where
  lo = fromIntegral $ (w `shiftL` 8) `shiftR` 8
  hi = (fromIntegral $ w `shiftR` 8) `setBit` 7 


