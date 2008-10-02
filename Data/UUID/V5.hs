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
-- This module implements Version 5 UUIDs as specified
-- in RFC 4122.
--
-- These UUIDs identify an object within a namespace,
-- and are deterministic.
--
-- The namespace is identified by a UUID.  Several sample
-- namespaces are enclosed.

module Data.UUID.V5
    (generateNamed
    ,namespaceDNS
    ,namespaceURL
    ,namespaceOID
    ,namespaceX500
    ) where
                    

import Data.UUID.Internal

import Data.Binary
import Data.Bits
import Data.Maybe

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.Digest.SHA1 as SHA1


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

versionSHA = shiftL 12 5

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
