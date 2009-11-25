{-# LANGUAGE DeriveDataTypeable, CPP #-}

-- |
-- Module      : Data.UUID
-- Copyright   : (c) 2008 Antoine Latter
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable

module Data.UUID.Internal
    (UUID(..)
    ,null
    ,nil
    ,Node(..)
    ,nullNode
    ,nodeToList
    ,listToNode
    ,fromByteString
    ,toByteString
    ,fromString
    ,toString
    ,versionMask
    ,reservedMask
    ,reserved
    ) where

import Prelude hiding (null)
import qualified Prelude

import Data.Word
import Data.Char
import Data.Maybe
import Data.Bits
import Data.List (splitAt, foldl', unfoldr)

import Data.Typeable

#if MIN_VERSION_base(4,0,0)
import Data.Data
#else
import Data.Generics.Basics
#endif

import Foreign.Ptr
import Foreign.Storable

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as Lazy

import System.Random

import Text.Printf

#ifndef STRICT
#define SLOT(x) x
#else
#define SLOT(x) {-# UNPACK #-} !x
#endif

-- |The UUID type.  A 'Random' instance is provided which produces
-- version 4 UUIDs as specified in RFC 4122.  The 'Storable' and 
-- 'Binary' instances are compatable with RFC 4122.  The 'Binary'
-- instance serializes to network byte order.
data UUID = UUID
    {uuid_timeLow  :: SLOT(Word32)
    ,uuid_timeMid  :: SLOT(Word16)
    ,uuid_timeHigh :: SLOT(Word16) -- includes version number
    ,uuid_clockSeqHi :: SLOT(Word8) -- includes reserved field
    ,uuid_clokcSeqLow :: SLOT(Word8)
    ,uuid_node :: SLOT(Node)
    } deriving (Eq, Ord, Typeable)

-- |Returns true if the passed-in UUID is the 'nil' UUID.
null :: UUID -> Bool
null (UUID 0 0 0 0 0 node) | nullNode node = True
null _ = False

-- |The nil UUID, as defined in RFC 4122.
-- It is a UUID of all zeros. @'null' u@ iff @'u' == 'nil'@.
nil :: UUID
nil = UUID 0 0 0 0 0 (Node 0 0 0 0 0 0)

instance Random UUID where
    random g = let (timeLow, g1)  = randomBoundedIntegral g
                   (timeMid, g2)  = randomBoundedIntegral g1
                   (timeHigh, g3) = randomBoundedIntegral g2
                   (seqHigh, g4)  = randomBoundedIntegral g3
                   (seqLow, g5)   = randomBoundedIntegral g4
                   (node, g6)     = random g5
                   seqHighReserved = (seqHigh .&. reservedMask) .|. reserved
                   timeHighVersion = (timeHigh .&. versionMask) .|. versionRandom
               in (UUID timeLow timeMid timeHighVersion seqHighReserved seqLow node, g6)

    randomR _ = random -- range is ignored

versionMask :: Word16 -- 0000 1111 1111 1111
versionMask = 0x0FFF

versionRandom :: Word16
versionRandom = 4 `shiftL` 12

reservedMask :: Word8 -- 0011 1111
reservedMask = 0x3F

reserved :: Word8
reserved = bit 7

data Node = Node
    SLOT(Word8)
    SLOT(Word8)
    SLOT(Word8)
    SLOT(Word8)
    SLOT(Word8)
    SLOT(Word8)
 deriving (Eq, Ord, Typeable)

instance Random Node where
    random g = let (w1, g1) = randomBoundedIntegral g
                   (w2, g2) = randomBoundedIntegral g1
                   (w3, g3) = randomBoundedIntegral g2
                   (w4, g4) = randomBoundedIntegral g3
                   (w5, g5) = randomBoundedIntegral g4
                   (w6, g6) = randomBoundedIntegral g5
               in (Node w1 w2 w3 w4 w5 w6, g6)
    randomR _ = random -- neglect range

nullNode :: Node -> Bool
nullNode (Node 0 0 0 0 0 0) = True
nullNode _ = False

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


-- |Extract a UUID from a 'ByteString' in network byte order.
-- The argument must be 16 bytes long, otherwise 'Nothing' is returned.
fromByteString :: Lazy.ByteString -> Maybe UUID
fromByteString bs | Lazy.length bs == 16 = Just $ decode bs
                  | otherwise            = Nothing

-- |Encode a UUID into a 'ByteString' in network order.
toByteString :: UUID -> Lazy.ByteString
toByteString uu = encode uu

-- My goal with this instance was to make it work just enough to do what
-- I want when used with the HStringTemplate library.
instance Data UUID where
    toConstr uu  = mkConstr uuidType (show uu) [] (error "fixity")
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = uuidType

uuidType =  mkNorepType "Data.UUID.UUID"




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


-- | Convert a UUID into a hypenated string using lower-case letters.
-- Example:
--
-- @
--  toString $ fromString \"550e8400-e29b-41d4-a716-446655440000\"
-- @
toString :: UUID -> String
toString (UUID tl tm th ch cl n) = printf "%08x-%04x-%04x-%02x%02x-%s" tl tm th ch cl ns
    where ns = concatMap hexb $ nodeToList n
          hexb x = printf "%02x" x :: String


-- remove all occurances of the input element in the inpt list.
-- none of the sub-lists are empty.
splitList :: Eq a => a -> [a] -> [[a]]
splitList c xs = let ys = dropWhile (== c) xs
                 in case span (/= c) ys of
                      ([],_) -> []
                      (sub,rest) -> sub : splitList c rest

-- the passed-in predicate signals when to stop unfolding
unfoldUntil :: (b -> Bool) -> (b -> (a, b)) -> b -> [a]
unfoldUntil p f n = unfoldr g n
 where g m | p m       = Nothing
           | otherwise = Just $ f m


-- no random intance for Data.Word types :-(
-- this will work, though

randomBoundedIntegral :: (RandomGen g, Bounded a, Integral a) => g -> (a, g)
randomBoundedIntegral g =
    let (n, g1) = randomR (fromIntegral l, fromIntegral u) g
        _ = n :: Integer
        retVal = fromIntegral n `asTypeOf` (l `asTypeOf` u)
        u = maxBound
        l = minBound
    in (retVal, g1)

