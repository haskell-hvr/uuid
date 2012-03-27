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
import Data.Bits
import qualified Data.ByteString as B

import Data.UUID.Internal
import qualified Data.UUID.Named as Shared
import qualified Crypto.Hash.SHA1 as SHA1


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
           = case B.unpack $ SHA1.hash $ B.pack bytes of
               a1:b1:c1:d1: a2:b2:c2:d2: a3:b3:c3:d3: a4:b4:c4:d4:_ ->
                   ( toWord a1 b1 c1 d1
                   , toWord a2 b2 c2 d2
                   , toWord a3 b3 c3 d3
                   , toWord a4 b4 c4 d4
                   )
               _ -> error "Data.UUID.V5.hash: fatal error"

toWord :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
toWord a b c d =     fromIntegral a `shiftL` 24
                 .|. fromIntegral b `shiftL` 16
                 .|. fromIntegral c `shiftL` 8
                 .|. fromIntegral d
