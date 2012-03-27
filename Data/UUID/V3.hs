-- |
-- Module      : Data.UUID.V3
-- Copyright   : (c) 2010 Antoine Latter
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
--
--
-- This module implements Version 3 UUIDs as specified
-- in RFC 4122.
--
-- These UUIDs identify an object within a namespace,
-- and are deterministic.
--
-- The namespace is identified by a UUID.  Several sample
-- namespaces are enclosed.

module Data.UUID.V3
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
import qualified Crypto.Hash.MD5 as MD5


-- |Generate a 'UUID' within the specified namespace out of the given
-- object.
--
-- Uses an MD5 hash. The UUID is built from first 128 bits of the hash of
-- the namespace UUID and the name (as a series of Word8).
generateNamed :: UUID    -- ^Namespace
              -> [Word8] -- ^Object
              -> UUID
generateNamed = Shared.generateNamed hash 3

hash :: [Word8] -> (Word32, Word32, Word32, Word32)
hash bytes
    = case map f $ chunk 4 $ B.unpack $ MD5.hash $ B.pack bytes of
        w1:w2:w3:w4:_ -> (w1, w2, w3, w4)
        _ -> error "Data.UUID.V3.hash: fatal error"

 where f [b1, b2, b3, b4]
           = sum
             [ fromIntegral b4
             , fromIntegral b3 `shiftL` 8
             , fromIntegral b2 `shiftL` 16
             , fromIntegral b1 `shiftL` 24
             ]
       f _ = error "Data.UUID.V3.hash: fatal error"

chunk :: Int -> [a] -> [[a]]
chunk n = go n []
 where go _ [] [] = []
       go _ ys [] = reverse ys:[]
       go 0 ys xs = reverse ys:go n [] xs
       go m ys (x:xs) = go (m-1) (x:ys) xs
