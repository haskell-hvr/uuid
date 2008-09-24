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
      tm   <- peekByteOff (castPtr p) 4
      th   <- peekByteOff (castPtr p) 6
      ch   <- peekByteOff (castPtr p) 8
      cl   <- peekByteOff (castPtr p) 9
      node <- peekByteOff (castPtr p) 10
      return $ UUID tl tm th ch cl node

    poke p (UUID tl tm th ch cl node) = do
      poke (castPtr p) tl
      pokeByteOff (castPtr p) 4 tm
      pokeByteOff (castPtr p) 6 th
      pokeByteOff (castPtr p) 8 ch
      pokeByteOff (castPtr p) 9 cl
      pokeByteOff (castPtr p) 10 node

instance Storable Node where
    sizeOf _ = 6
    alignment _ = 1 -- ???

    peek p = do
      w1 <- peek $ castPtr p
      w2 <- peekByteOff (castPtr p) 1
      w3 <- peekByteOff (castPtr p) 2
      w4 <- peekByteOff (castPtr p) 3
      w5 <- peekByteOff (castPtr p) 4
      w6 <- peekByteOff (castPtr p) 5
      return $ Node w1 w2 w3 w4 w5 w6

    poke p (Node w1 w2 w3 w4 w5 w6) = do
      poke (castPtr p) w1
      pokeByteOff (castPtr p) 1 w2
      pokeByteOff (castPtr p) 2 w3
      pokeByteOff (castPtr p) 3 w4
      pokeByteOff (castPtr p) 4 w5
      pokeByteOff (castPtr p) 5 w6

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
 where g m = case p m of
               True -> Nothing
               False -> Just $ f m

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
