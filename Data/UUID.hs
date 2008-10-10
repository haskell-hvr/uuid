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
--
-- For generating UUIDs, check out 'Data.UUID.V1', 'Data.UUID.V5' and
-- 'System.Random'.

module Data.UUID(UUID
                ,toString
                ,fromString
                ,null
                ) where

import Data.UUID.Internal

-- Everything is really implemented in Data.UUID.Internal,
-- but I don't want to export the constructors out of the
-- package.