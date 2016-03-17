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

import Data.UUID.Types.Internal

import Control.Applicative ((<*>),(<$>))
import Data.Binary.Get (runGet, getWord32be)
import Data.Maybe
import Data.Word (Word8)

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
generateNamed :: BA.ByteArrayAccess bs
              => (B.ByteString -> bs) -- ^Hash
              -> Word8   -- ^Version
              ->  UUID   -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed hash version namespace object =
    let chunk = B.pack $ toList namespace ++ object
        bytes = BL.fromStrict . BA.convert $ hash chunk
        w = getWord32be
        unpackBytes = runGet $
         buildFromWords version <$> w <*> w <*> w <*> w
    in unpackBytes bytes


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
