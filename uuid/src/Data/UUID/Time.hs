{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE TypeFamilies #-}
module Data.UUID.Time(stepTime, MACSource(..),(/-/))
where

import Data.Maybe
import Data.Time
import Data.Word
import Data.Bits
import Data.List

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent.MVar
import System.IO.Unsafe

import qualified System.Random as R

import Network.Info
import Data.UUID.Types.Internal.Builder

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
    return . find (minBound /=) . map mac >>=
    maybe randomMac return

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
