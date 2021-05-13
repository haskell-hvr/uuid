{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Data.UUID.V1
Copyright   : (c) 2008 Jason Dusek
              (c) 2009 Mark Lentczner
              (c) 2009-2010,2012 Antoine Latter

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable

RFC 4122 Version 1 UUID state machine.

The generated UUID is based on the hardware MAC
address and the system clock.

If we cannot lookup the MAC address we seed the
generator with a pseudo-random number.
-}

module Data.UUID.V1(nextUUID)
where


import Data.Bits
import Data.Maybe
import Data.Time
import Data.Word

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent.MVar
import System.IO.Unsafe

import qualified System.Random as R

import Network.Info

import Data.UUID.Types.Internal.Builder
import Data.UUID.Types.Internal

-- | Returns a new UUID derived from the local hardware MAC
-- address and the current system time.
-- Is generated according to the Version 1 UUID specified in
-- RFC 4122.
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
    buildFromBytes 1 /-/ tLow /-/ tMid /-/ tHigh /-/ clock /-/ (MACSource mac')
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
stepTime :: IO (Maybe (MAC, Word16, Word64))
stepTime = do
  h1 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  modifyMVar state $ \s@(State mac' c0 h0) ->
   if h1 > h0
    then
      return (State mac' c0 h1, Just (mac', c0, h1))
    else
      let
        c1 = succ c0
      in if c1 <= 0x3fff -- when clock is initially randomized,
                      -- then this test will need to change
         then
          return (State mac' c1 h1, Just (mac', c1, h1))
        else
          return (s, Nothing)


{-# NOINLINE state #-}
state :: MVar State
state = unsafePerformIO $ do
  h0 <- fmap hundredsOfNanosSinceGregorianReform getCurrentTime
  mac' <- getMac
  newMVar $ State mac' 0 h0 -- the 0 should be a random number

-- SysMAC.mac can fail on some machines.
-- In those cases we fake it with a random
-- 6 bytes seed.
getMac :: IO MAC
getMac =
    getNetworkInterfaces >>=
    return . listToMaybe . filter (minBound /=) . map mac >>=
    \macM -> case macM of
      Just m -> return m
      Nothing -> randomMac

randomMac :: IO MAC
randomMac =
    -- I'm too lazy to thread through
    -- the random state ...
    MAC
     <$> (R.randomIO >>= return . (1 .|.)) -- We must set the multicast bit to True. See section 4.5 of the RFC.
     <*> R.randomIO
     <*> R.randomIO
     <*> R.randomIO
     <*> R.randomIO
     <*> R.randomIO

data State = State
    {-# UNPACK #-} !MAC
    {-# UNPACK #-} !Word16
    {-# UNPACK #-} !Word64
 deriving (Show)



hundredsOfNanosSinceGregorianReform :: UTCTime -> Word64
hundredsOfNanosSinceGregorianReform t = floor $ 10000000 * dt
 where
  gregorianReform = UTCTime (fromGregorian 1582 10 15) 0
  dt = t `diffUTCTime` gregorianReform
