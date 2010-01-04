{-# LANGUAGE DeriveDataTypeable, TypeFamilies, CPP #-}

-- |
-- Module      : Data.UUID
-- Copyright   : (c) 2008-2009 Antoine Latter
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
    ,fromByteString
    ,toByteString
    ,fromString
    ,toString
    ,buildFromBytes
    ,buildFromWords
    ) where

import Prelude hiding (null)
import qualified Prelude

import Control.Monad (liftM4)
import Data.Char
import Data.Maybe
import Data.Bits
import Data.List (elemIndices)

#if MIN_VERSION_base(4,0,0)
import Data.Data
#else
import Data.Generics.Basics
#endif

import Foreign.Storable

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as Lazy

import Data.UUID.Builder

import System.Random


-- |The UUID type.  A 'Random' instance is provided which produces
-- version 4 UUIDs as specified in RFC 4122.  The 'Storable' and
-- 'Binary' instances are compatible with RFC 4122, storing the fields
-- in network order as 16 bytes.
data UUID = UUID !Word32 !Word32 !Word32 !Word32
    deriving (Eq, Ord, Typeable)
{-
    Other representations that we tried are:
         Mimic V1 structure:     !Word32 !Word16 !Word16 !Word16
                                   !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
         Sixteen bytes:          !Word8 ... (x 16)
         Simple list of bytes:   [Word8]
         ByteString (strict)     ByteString
         Immutable array:        UArray Int Word8
         Vector:                 UArr Word8
    None was as fast, overall, as the representation used here.
-}


--
-- UTILITIES
--

-- |Build a Word32 from four Word8 values, presented in big-endian order
word :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word a b c d =  (fromIntegral a `shiftL` 24)
            .|. (fromIntegral b `shiftL` 16)
            .|. (fromIntegral c `shiftL`  8)
            .|. (fromIntegral d            )

-- |Extract a Word8 from a Word32. Bytes, high to low, are numbered from 3 to 0,
byte :: Int -> Word32 -> Word8
byte i w = fromIntegral (w `shiftR` (i * 8))


-- |Make a UUID from sixteen Word8 values
makeFromBytes :: Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> UUID
makeFromBytes b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
        = UUID w0 w1 w2 w3
    where w0 = word b0 b1 b2 b3
          w1 = word b4 b5 b6 b7
          w2 = word b8 b9 ba bb
          w3 = word bc bd be bf

-- |Make a UUID from four Word32 values
makeFromWords :: Word32 -> Word32 -> Word32 -> Word32 -> UUID
makeFromWords = UUID

-- |A Builder for constructing a UUID of a given version.
buildFromBytes :: Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> UUID
buildFromBytes v b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf =
    makeFromBytes b0 b1 b2 b3 b4 b5 b6' b7 b8' b9 ba bb bc bd be bf
    where b6' = b6 .&. 0x0f .|. (v `shiftL` 4)
          b8' = b8 .&. 0x3f .|. 0x80

-- |Build a UUID of a given version from Word32 values.
buildFromWords :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> UUID
buildFromWords v w0 w1 w2 w3 = makeFromWords w0 w1' w2' w3
    where w1' = w1 .&. 0xffff0fff .|. (v `shiftL` 12)
          w2' = w2 .&. 0x3fffffff .|. 0x80000000


-- |Return the bytes that make up the UUID
toList :: UUID -> [Word8]
toList (UUID w0 w1 w2 w3) =
    [byte 3 w0, byte 2 w0, byte 1 w0, byte 0 w0,
     byte 3 w1, byte 2 w1, byte 1 w1, byte 0 w1,
     byte 3 w2, byte 2 w2, byte 1 w2, byte 0 w2,
     byte 3 w3, byte 2 w3, byte 1 w3, byte 0 w3]

-- |Construct a UUID from a list of Word8. Returns Nothing if the list isn't
-- exactly sixteen bytes long
fromList :: [Word8] -> Maybe UUID
fromList [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] =
    Just $ makeFromBytes b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
fromList _ = Nothing


--
-- UUID API
--

-- |Returns true if the passed-in UUID is the 'nil' UUID.
null :: UUID -> Bool
null = (== nil)
    -- Note: This actually faster than:
    --      null (UUID 0 0 0 0) = True
    --      null _              = False

-- |The nil UUID, as defined in RFC 4122.
-- It is a UUID of all zeros. @'null' u@ iff @'u' == 'nil'@.
nil :: UUID
nil = UUID 0 0 0 0

-- |Extract a UUID from a 'ByteString' in network byte order.
-- The argument must be 16 bytes long, otherwise 'Nothing' is returned.
fromByteString :: Lazy.ByteString -> Maybe UUID
fromByteString = fromList . Lazy.unpack

-- |Encode a UUID into a 'ByteString' in network order.
toByteString :: UUID -> Lazy.ByteString
toByteString = Lazy.pack . toList

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
fromString xs | validFmt  = fromString' xs
              | otherwise = Nothing
  where validFmt = elemIndices '-' xs == [8,13,18,23]

fromString' :: String -> Maybe UUID
fromString' s0 = do
    (w0, s1) <- hexWord s0
    (w1, s2) <- hexWord s1
    (w2, s3) <- hexWord s2
    (w3, s4) <- hexWord s3
    if s4 /= "" then Nothing
                else Just $ UUID w0 w1 w2 w3
    where hexWord :: String -> Maybe (Word32, String)
          hexWord s = Just (0, s) >>= hexByte >>= hexByte
                                  >>= hexByte >>= hexByte

          hexByte :: (Word32, String) -> Maybe (Word32, String)
          hexByte (w, '-':ds) = hexByte (w, ds)
          hexByte (w, hi:lo:ds)
              | bothHex   = Just ((w `shiftL` 8) .|. octet, ds)
              | otherwise = Nothing
              where bothHex = isHexDigit hi && isHexDigit lo
                    octet = fromIntegral (16 * digitToInt hi + digitToInt lo)
          hexByte _ = Nothing

-- | Convert a UUID into a hypenated string using lower-case letters.
-- Example:
--
-- @
--  toString $ fromString \"550e8400-e29b-41d4-a716-446655440000\"
-- @
toString :: UUID -> String
toString (UUID w0 w1 w2 w3) = hexw w0 $ hexw' w1 $ hexw' w2 $ hexw w3 ""
    where hexw :: Word32 -> String -> String
          hexw  w s = hexn w 28 : hexn w 24 : hexn w 20 : hexn w 16
                    : hexn w 12 : hexn w  8 : hexn w  4 : hexn w  0 : s

          hexw' :: Word32 -> String -> String
          hexw' w s = '-' : hexn w 28 : hexn w 24 : hexn w 20 : hexn w 16
                    : '-' : hexn w 12 : hexn w  8 : hexn w  4 : hexn w  0 : s

          hexn :: Word32 -> Int -> Char
          hexn w r = intToDigit $ fromIntegral ((w `shiftR` r) .&. 0xf)


--
-- Class Instances
--

instance Random UUID where
    random g = (fromGenNext w0 w1 w2 w3 w4, g4)
        where (w0, g0) = next g
              (w1, g1) = next g0
              (w2, g2) = next g1
              (w3, g3) = next g2
              (w4, g4) = next g3
    randomR _ = random -- range is ignored

-- |Build a UUID from the results of five calls to next on a StdGen.
-- While next on StdGet returns an Int, it doesn't provide 32 bits of
-- randomness. This code relies on at last 28 bits of randomness in the
-- and optimizes its use so as to make only five random values, not six.
fromGenNext :: Int -> Int -> Int -> Int -> Int -> UUID
fromGenNext w0 w1 w2 w3 w4 =
    buildFromBytes 4 /-/ (ThreeByte w0)
                     /-/ (ThreeByte w1)
                     /-/ w2    -- use all 4 bytes because we know the version
                               -- field will "cover" the upper, non-random bits
                     /-/ (ThreeByte w3)
                     /-/ (ThreeByte w4)

-- |A ByteSource to extract only three bytes from an Int, since next on StdGet
-- only returns 31 bits of randomness.
type instance ByteSink ThreeByte g = Takes3Bytes g
newtype ThreeByte = ThreeByte Int
instance ByteSource ThreeByte where
    f /-/ (ThreeByte w) = f b1 b2 b3
        where b1 = fromIntegral (w `shiftR` 16)
              b2 = fromIntegral (w `shiftR` 8)
              b3 = fromIntegral w


instance Show UUID where
    show = toString

instance Read UUID where
    readsPrec _ str = case fromString (take 36 str) of
      Nothing -> []
      Just u  -> [(u,drop 36 str)]


instance Storable UUID where
    sizeOf _ = 16
    alignment _ = 1 -- UUIDs are stored as individual octets

    peekByteOff p off =
      mapM (peekByteOff p) [off..(off+15)] >>= return . fromJust . fromList 

    pokeByteOff p off u =
      sequence_ $ zipWith (pokeByteOff p) [off..] (toList u)


instance Binary UUID where
    put (UUID w0 w1 w2 w3) =
        putWord32be w0 >> putWord32be w1 >> putWord32be w2 >> putWord32be w3 
    get = liftM4 UUID getWord32be getWord32be getWord32be getWord32be


-- My goal with this instance was to make it work just enough to do what
-- I want when used with the HStringTemplate library.
instance Data UUID where
    toConstr uu  = mkConstr uuidType (show uu) [] (error "fixity")
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = uuidType

uuidType :: DataType
uuidType =  mkNoRepType "Data.UUID.UUID"

#if !(MIN_VERSION_base(4,2,0))
mkNoRepType :: String -> DataType
mkNoRepType = mkNorepType
#endif

