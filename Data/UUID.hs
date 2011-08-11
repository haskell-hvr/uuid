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
-- See <http://tools.ietf.org/html/rfc4122> for the specification.
--
-- For generating UUIDs, check out 'Data.UUID.V1', 'Data.UUID.V5' and
-- 'System.Random'.

module Data.UUID(UUID
                ,toString
                ,fromString
                ,toByteString
                ,fromByteString
                ,toWords
                ,fromWords
                ,null
                ,nil
                ) where

import Prelude () -- we need to hide Prelude.null
import Data.UUID.Internal

-- Everything is really implemented in Data.UUID.Internal,
-- but I don't want to export the constructors out of the
-- package.
