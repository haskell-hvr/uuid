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

module Data.UUID.V1(nextUUID)
where

import Data.Time

import Data.Bits
import Data.Word

import Control.Concurrent.MVar
import System.IO
import System.IO.Unsafe

import qualified System.Info.MAC as SysMAC
import Data.MAC

import Data.UUID.Builder
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
  mac <- SysMAC.mac
  case (res, mac) of
    (Just (c, t), Just m) -> return $ Just $ makeUUID t c m
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


-- |Approximates the clock algorithm in RFC 4122, section 4.2
-- Isn't system wide or thread safe, nor does it properly randomize
-- the clock value on initialization.
stepTime :: IO (Maybe (Word16, Word64))
stepTime = do
  h1 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  modifyMVar state $ \s@(State c0 h0) ->
   if h1 > h0
    then
      return (State c0 h1, Just (c0, h1))
    else
      let
        c1 = succ c0
      in if c1 <= 0x3fff -- when clock is initially randomized,
                      -- then this test will need to change
         then
          return (State c1 h1, Just (c1, h1))
        else
          return (s, Nothing)


{-# NOINLINE state #-}
state :: MVar State
state = unsafePerformIO $ do
  h0 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  newMVar $ State 0 h0 -- the 0 should be a random number


data State = State
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word64
 deriving (Show)



hundredsOfNanosSinceGregorianReform :: UTCTime -> Word64
hundredsOfNanosSinceGregorianReform t = floor $ 10000000 * dt
 where
  gregorianReform = UTCTime (fromGregorian 1582 10 15) 0
  dt = t `diffUTCTime` gregorianReform



