{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.UUID
-- Copyright   : (c) 2008 Antoine Latter
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- This library is useful for creating, comparing, parsing and
-- printing Universally Unique Identifiers.
-- See <http://en.wikipedia.org/wiki/UUID> for the general idea.

module Data.UUID(UUID
                ,fromString
                ,toString
                ,toStringUpper
                ,toStringLower
                ,generate
                ,generateRandom
                ,generateTime
                ,generateNamed
                ,namespaceDNS
                ,namespaceURL
                ,namespaceOID
                ,namespaceX500
                ,null
                )
where

import Data.Word
import System.Random
import Data.Maybe

import Data.Char

import Data.Typeable
import Data.Generics.Basics

import Data.Bits

import Text.Printf

import Prelude hiding (null)
import qualified Prelude

import Data.List (splitAt, foldl', unfoldr)

import Foreign.Ptr
import Foreign.Storable

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.Digest.SHA1 as SHA1

data UUID = UUID
    {uuid_timeLow  :: {-# UNPACK #-} !Word32
    ,uuid_timeMid  :: {-# UNPACK #-} !Word16
    ,uuid_timeHigh :: {-# UNPACK #-} !Word16 -- includes version number
    ,uuid_clockSeqHi :: {-# UNPACK #-} !Word8 -- includes reserved field
    ,uuid_clokcSeqLow :: {-# UNPACK #-} !Word8
    ,uuid_node :: {-# UNPACK #-} !Node
    } deriving (Eq, Ord, Typeable)

instance Random UUID where
    random g = let (timeLow, g1)  = random g
                   (timeMid, g2)  = random g1
                   (timeHigh, g3) = random g2
                   (seqHigh, g4)  = random g3
                   (seqLow, g5)   = random g4
                   (node, g6)     = random g5
                   seqHighReserved = (seqHigh .&. reservedMask) .|. reserved
                   timeHighVersion = (timeHigh .&. versionMask) .|. versionRandom
               in (UUID timeLow timeMid timeHighVersion seqHighReserved seqLow node, g6)

    randomR _ = random -- range is ignored

versionMask :: Word16 -- 0000 1111 1111 1111
versionMask = 0x0FFF

versionRandom :: Word16
versionRandom = shiftL 12 4

versionSHA :: Word16
versionSHA = shiftL 12 5

reservedMask :: Word8 -- 0011 1111
reservedMask = 0x3F

reserved :: Word8
reserved = bit 7

data Node = Node
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
 deriving (Eq, Ord, Typeable)

instance Random Node where
    random g = let (w1, g1) = random g
                   (w2, g2) = random g1
                   (w3, g3) = random g2
                   (w4, g4) = random g3
                   (w5, g5) = random g4
                   (w6, g6) = random g5
               in (Node w1 w2 w3 w4 w5 w6, g6)
    randomR _ = random -- neglect range


nodeToList :: Node -> [Word8]
nodeToList (Node w1 w2 w3 w4 w5 w6) = [w1, w2, w3, w4, w5, w6]

listToNode :: [Word8] -> Maybe Node
listToNode [w1, w2, w3, w4, w5, w6] = return $ Node w1 w2 w3 w4 w5 w6
listToNode _ = Nothing

instance Show UUID where
    show = toString

instance Read UUID where
    readsPrec _ str = case fromString (take 36 str) of
      Nothing -> []
      Just u  -> [(u,drop 36 str)]


instance Storable UUID where
    sizeOf _ = 16
    alignment _ = 4 -- not sure what to put here

    peek p = do
      tl   <- peek $ castPtr p
      tm   <- peekByteOff p 4
      th   <- peekByteOff p 6
      ch   <- peekByteOff p 8
      cl   <- peekByteOff p 9
      node <- peekByteOff p 10
      return $ UUID tl tm th ch cl node

    poke p (UUID tl tm th ch cl node) = do
      poke (castPtr p) tl
      pokeByteOff p 4 tm
      pokeByteOff p 6 th
      pokeByteOff p 8 ch
      pokeByteOff p 9 cl
      pokeByteOff p 10 node

instance Storable Node where
    sizeOf _ = 6
    alignment _ = 1 -- ???

    peek p = do
      w1 <- peek $ castPtr p
      w2 <- peekByteOff p 1
      w3 <- peekByteOff p 2
      w4 <- peekByteOff p 3
      w5 <- peekByteOff p 4
      w6 <- peekByteOff p 5
      return $ Node w1 w2 w3 w4 w5 w6

    poke p (Node w1 w2 w3 w4 w5 w6) = do
      poke (castPtr p) w1
      pokeByteOff p 1 w2
      pokeByteOff p 2 w3
      pokeByteOff p 3 w4
      pokeByteOff p 4 w5
      pokeByteOff p 5 w6

-- Binary instance in network byte-order
instance Binary UUID where
    put (UUID tl tm th ch cl n) = do
                       putWord32be tl
                       putWord16be tm
                       putWord16be th
                       putWord8 ch
                       putWord8 cl
                       put n

    get = do
      tl <- getWord32be
      tm <- getWord16be
      th <- getWord16be
      ch <- getWord8
      cl <- getWord8
      node <- get
      return $ UUID tl tm th ch cl node

instance Binary Node where
    put (Node w1 w2 w3 w4 w5 w6) = do
                       putWord8 w1
                       putWord8 w2
                       putWord8 w3
                       putWord8 w4
                       putWord8 w5
                       putWord8 w6

    get = do
      w1 <- getWord8
      w2 <- getWord8
      w3 <- getWord8
      w4 <- getWord8
      w5 <- getWord8
      w6 <- getWord8
      return $ Node w1 w2 w3 w4 w5 w6


-- My goal with this instance was to make it work just enough to do what
-- I want when used with the HStringTemplate library.
instance Data UUID where
    toConstr uu  = mkConstr uuidType (show uu) [] (error "fixity")
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = uuidType

uuidType =  mkNorepType "Data.UUID.UUID"


-- |Creates a new 'UUID'.  If \/dev\/urandom is available, it will be used.
-- Otherwise a UUID will be generated based on the current time and the
-- hardware MAC address, if available.
generate :: IO UUID
generate = generateRandom

-- |Create a new 'UUID'.  If \/dev\/urandom is available, it will be used.
-- Otherwise a psuedorandom generator will be used.
generateRandom :: IO UUID
generateRandom = randomIO  -- not a good solution, as someone could have changed the global generator

-- |Create a new 'UUID'.  The UUID will be  generated based on the
-- current time and the hardware MAC address, if available.
generateTime :: IO UUID
generateTime = error "Data.UUID.generateTime: not yet implemented"

-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
--
-- Uses a SHA1 hash.
generateNamed :: UUID    -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed namespace object =
    let chunk = BS.unpack (encode namespace) ++ object
        SHA1.Word160 w1 w2 w3 w4 w5 = SHA1.hash chunk

        tl = w1
        tm = low16 w2
        th = (versionSHA .|.) $ (versionMask .&.) $ high16 w2
        ch = (reserved .|.) $ (reservedMask .&.) $ high8 $ high16 w3
        cl = low8  $ high16 w3
        node = Node (high8 (low16 w3))
                    (low8  (low16 w3))
                    (high8 (high16 w4))
                    (low8 (high16 w4))
                    (high8 (low16 w4))
                    (low8 (low16 w4))
    in UUID tl tm th ch cl node

-- HASH 0  1  - 2  3  : w1
--      4  5  - 6  7  : w2
--      8  9  - 10 11 : w3
--      12 13 - 14 15 : w4
--      16 17 - 18 19 : w5

low16 :: Word32 -> Word16
low16 = fromIntegral . (.&. 0x0000FFFF)

high16 :: Word32 -> Word16
high16 = fromIntegral . flip shiftR 16

low8 :: Word16 -> Word8
low8 = fromIntegral . (.&. 0x00FF)

high8 :: Word16 -> Word8
high8 = fromIntegral . flip shiftR 4

-- this may be overkill ...
{-# RULES
  "low8/low16"    forall x . low8  (low16 x)  = fromIntegral (x .&. 0x000000FF)
  "high8/low16"   forall x . high8 (low16 x)  = fromIntegral (shiftR (x .&. 0x0000FF00) 4)
  "low8/high16"   forall x . low8  (high16 x) = fromIntegral (shiftR (x .&. 0x00FF0000) 8)
  "high8/high16"  forall x . high8 (high16 x) = fromIntegral (shiftR x 12)
  #-}

unsafeFromString :: String -> UUID
unsafeFromString = fromJust . fromString

-- |The namespace for DNS addresses
namespaceDNS :: UUID
namespaceDNS = unsafeFromString "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

-- |The namespace for URLs
namespaceURL :: UUID
namespaceURL = unsafeFromString "6ba7b811-9dad-11d1-80b4-00c04fd430c8"

namespaceOID :: UUID
namespaceOID = unsafeFromString "6ba7b812-9dad-11d1-80b4-00c04fd430c8"

namespaceX500 :: UUID
namespaceX500 = unsafeFromString "6ba7b814-9dad-11d1-80b4-00c04fd430c8"

-- |Returns 'True' if the passed-in 'UUID' is the null UUID.
null :: UUID -> Bool
null = (== nullUuid)
 where nullUuid = UUID 0 0 0 0 0 $ Node 0 0 0 0 0 0


-- |If the passed in 'String' can be parsed as a 'UUID', it will be.
-- The hyphens may not be omitted.
-- Example:
--
-- @
--  fromString \"c2cc10e1-57d6-4b6f-9899-38d972112d8c\"
-- @
--
-- Hex digits may be upper or lower-case.
fromString :: String -> Maybe UUID
fromString xs | validFmt  = Just uuid
              | otherwise = Nothing
  where validFmt = length ws == 5 &&
                   map length ws == [8,4,4,4,12] &&
                   all isHexDigit (concat ws) &&
                   isJust node
        ws = splitList '-' xs
        [tl, tm, th, c, n] = ws
        ns = unfoldUntil Prelude.null (splitAt 2) n :: [String]
        node = listToNode $ map hexVal ns :: Maybe Node
        uuid = UUID (hexVal tl) (hexVal tm) (hexVal th) (hexVal $ take 2 c) (hexVal $ drop 2 c) (fromJust $ node)

-- | Convert a string to a hex value, assuming the string is already validated.
hexVal :: Num a => String -> a
hexVal = fromInteger . foldl' (\n c -> 16*n + digitToInteger c) 0

digitToInteger :: Char -> Integer
digitToInteger = fromIntegral . digitToInt

-- |Returns a 'String' representation of the passed in 'UUID'.
-- Hex digits occuring in the output will be either upper or
-- lower-case depending on system defaults and locale.
toString :: UUID -> String
toString = toStringLower


-- | Convert a UUID into a hypenated string using upper-case letters.
-- example: toStringUpper $ fromString "550e8400-e29b-41d4-a716-446655440000"
toStringUpper :: UUID -> String
toStringUpper (UUID tl tm th ch cl n) = printf "%08X-%04X-%04X-%02X%02X-%s" tl tm th ch cl ns
    where ns = concatMap hexb $ nodeToList n
          hexb x = printf "%02X" x :: String

-- | Convert a UUID into a hypenated string using lower-case letters.
-- example: toStringLower $ fromString "550e8400-e29b-41d4-a716-446655440000"
toStringLower :: UUID -> String
toStringLower (UUID tl tm th ch cl n) = printf "%08x-%04x-%04x-%02x%02x-%s" tl tm th ch cl ns
    where ns = concatMap hexb $ nodeToList n
          hexb x = printf "%02x" x :: String

-- remove all occurances of the input element in the inpt list.
-- non of the sub-lists are empty.
splitList :: Eq a => a -> [a] -> [[a]]
splitList c xs = let ys = dropWhile (== c) xs
                 in case span (/= c) ys of
                      ([],_) -> []
                      (sub,rest) -> sub : splitList c rest

unfoldUntil :: (b -> Bool) -> (b -> (a, b)) -> b -> [a]
unfoldUntil p f n = unfoldr g n
 where g m | p m       = Nothing
           | otherwise = Just $ f m

-- I'm not sure how fast these instances are, but they'll do for now.

instance Random Word8 where
    random g = randomBoundedIntegral g
    randomR r g = randomRIntegral r g

instance Random Word16 where
    random g = randomBoundedIntegral g
    randomR r g = randomRIntegral r g

instance Random Word32 where
    random g = randomBoundedIntegral g
    randomR r g = randomRIntegral r g

randomBoundedIntegral :: (RandomGen g, Bounded a, Integral a) => g -> (a, g)
randomBoundedIntegral g = let (n, g1) = randomR (fromIntegral l, fromIntegral u) g
                              _ = n :: Integer
                              retVal = fromIntegral n `asTypeOf` (l `asTypeOf` u)
                              u = maxBound
                              l = minBound
                          in (retVal, g1)

randomRIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomRIntegral (l, u) g = let (val, g1)  = randomR (fromIntegral l, fromIntegral u) g
                               _ = val :: Integer
                           in (fromIntegral val, g1)
