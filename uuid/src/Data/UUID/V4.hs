{- |
   Module      : Data.UUID.V4
   Copyright   : (c) 2012-2016 Antoine Latter

   License     : BSD-style

   Maintainer  : aslatter@gmail.com
   Stability   : experimental
   Portability : portable

   This module implements Version 4 UUIDs as specified
   in RFC 4122.

   These UUIDs are generated from a pseddo-random generator.
   We use 'getEntropy' method from <https://hackage.haskell.org/package/entropy entropy> package,
   which should provide cryptographically secure random data.
-}
module Data.UUID.V4 (nextRandom) where

import Data.UUID
import Data.UUID.Types.Internal ( buildFromBytes )

import System.Entropy ( getEntropy )
import Data.ByteString ( unpack )

-- | Generate a crytographically secure, random UUID. Introduced in version
-- 1.2.6.
nextRandom :: IO UUID
nextRandom = do
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf]
    <- unpack `fmap` getEntropy 16
  return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
