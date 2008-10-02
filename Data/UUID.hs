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
-- This library is useful for comparing, parsing and
-- printing Universally Unique Identifiers.
-- See <http://en.wikipedia.org/wiki/UUID> for the general idea.

module Data.UUID(UUID
                ,module Data.UUID.V1
                ,module Data.UUID.V5
                ,toString
                ,fromString
                ,null
                ) where

import Data.UUID.Internal
import Data.UUID.V1
import Data.UUID.V5

import Prelude hiding (null)

-- |Returns 'True' if the passed-in 'UUID' is the null UUID.
null :: UUID -> Bool
null = (== nullUuid)
 where nullUuid = UUID 0 0 0 0 0 $ Node 0 0 0 0 0 0
