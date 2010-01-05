-- |
-- Module      : Data.UUID.Named
-- Copyright   : (c) 2008 Antoine Latter
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
--
--
-- This module implements Version 3/5 UUIDs as specified
-- in RFC 4122.
--
-- These UUIDs identify an object within a namespace,
-- and are deterministic.
--
-- The namespace is identified by a UUID.  Several sample
-- namespaces are enclosed.

module Data.UUID.Named
    (generateNamed
    ,namespaceDNS
    ,namespaceURL
    ,namespaceOID
    ,namespaceX500
    ) where

import Data.UUID.Internal

import Data.Binary
import Data.Maybe

import qualified Data.ByteString.Lazy as BS

import qualified Data.Digest.SHA1 as SHA1


-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
generateNamed :: ([Word8] -> (Word32, Word32, Word32, Word32)) -- ^Hash
              -> Word8   -- ^Version
              ->  UUID   -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed hash version namespace object =
    let chunk = BS.unpack (toByteString namespace) ++ object
        (w1, w2, w3, w4) = hash chunk
    in buildFromWords version w1 w2 w3 w4



unsafeFromString :: String -> UUID
unsafeFromString = fromJust . fromString

-- |The namespace for DNS addresses
namespaceDNS :: UUID
namespaceDNS = unsafeFromString "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

-- |The namespace for URLs
namespaceURL :: UUID
namespaceURL = unsafeFromString "6ba7b811-9dad-11d1-80b4-00c04fd430c8"

-- |The namespace for ISO OIDs
namespaceOID :: UUID
namespaceOID = unsafeFromString "6ba7b812-9dad-11d1-80b4-00c04fd430c8"

-- |The namespace for X.500 DNs
namespaceX500 :: UUID
namespaceX500 = unsafeFromString "6ba7b814-9dad-11d1-80b4-00c04fd430c8"
