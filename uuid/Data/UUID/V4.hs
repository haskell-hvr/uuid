{- |
   Module      : Data.UUID.V4
   Copyright   : (c) 2012-2016 Antoine Latter

   License     : BSD-style

   Maintainer  : aslatter@gmail.com
   Stability   : experimental
   Portability : portable

   This module implements Version 4 UUIDs as specified
   in RFC 4122.

   These UUIDs are generated from a psuedo-random generator.
   We use the System.Random 'R.StdGen' as our random source.

   All of the logic is encapsulated in the 'R.Random' instance
   for the UUID type, so you are also free to use the random generator
   of your choice.
-}
module Data.UUID.V4 (nextRandom) where

import Data.UUID
import Data.UUID.Types.Internal ( buildFromBytes )

import System.Entropy ( getEntropy )
import Data.ByteString ( unpack )

-- | Generate a random UUID. Introduced in version 1.2.6.
nextRandom :: IO UUID
nextRandom = do
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf]
    <- unpack `fmap` getEntropy 16
  return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
