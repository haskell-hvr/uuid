{- |
Module      : Data.UUID.V3
Copyright   : (c) 2010,2012 Antoine Latter

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable

NOTE\: This module uses MD5 hashing. Unless you know
you need to use this module, you should probably be
using "Data.UUID.V5", which offers the same sort of
functionality as this module except implemented with
SHA-1 hashing.

This module implements Version 3 UUIDs as specified
in RFC 4122.

These UUIDs identify an object within a namespace,
and are deterministic.

The namespace is identified by a UUID.  Several sample
namespaces are enclosed.
-}
module Data.UUID.V3
    (generateNamed
    ,Shared.namespaceDNS
    ,Shared.namespaceURL
    ,Shared.namespaceOID
    ,Shared.namespaceX500
    ) where

import Data.Word

import Data.UUID.Types.Internal
import qualified Data.UUID.Named as Shared
import qualified Crypto.Hash.MD5 as MD5


-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
--
-- Uses an MD5 hash. The UUID is built from first 128 bits of the hash of
-- the namespace UUID and the name (as a series of Word8).
generateNamed :: UUID    -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed = Shared.generateNamed MD5.hash 3
