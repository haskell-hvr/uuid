-- |
-- Module      : Data.UUID.V5
-- Copyright   : (c) 2008-2009 Antoine Latter
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
    ,Shared.namespaceDNS
    ,Shared.namespaceURL
    ,Shared.namespaceOID
    ,Shared.namespaceX500
    ) where

import Data.Word

import Data.UUID.Internal
import qualified Data.UUID.Named as Shared
import qualified Data.Digest.SHA1 as SHA1


-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
--
-- Uses a SHA1 hash. The UUID is built from first 128 bits of the hash of
-- the namespace UUID and the name (as a series of Word8).
generateNamed :: UUID    -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed = Shared.generateNamed hash 5
 where hash bytes
           = case SHA1.hash bytes of
               SHA1.Word160 w1 w2 w3 w4 _w5 -> (w1, w2, w3, w4)
