-- |
-- Module      : Data.UUID.V5
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
import Data.Maybe

import qualified Data.ByteString.Lazy as BS

import qualified Data.Digest.SHA1 as SHA1


-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
--
-- Uses a SHA1 hash. The UUID is built from first 128 bits of the hash of
-- the namespace UUID and the name (as a series of Word8).
generateNamed :: UUID    -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed namespace object =
    let chunk = BS.unpack (toByteString namespace) ++ object
        SHA1.Word160 w1 w2 w3 w4 _w5 = SHA1.hash chunk
    in buildFromWords 5 w1 w2 w3 w4



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
