{-# LANGUAGE Trustworthy #-}

{- |
Module      : Data.UUID.Types
Copyright   : (c) 2017-2018 Herbert Valerio Riedel
              (c) 2008,2012 Antoine Latter

License     : BSD-style

Maintainer  : hvr@gnu.org
Portability : portable

This library is useful for comparing, parsing and
printing <http://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)>.
See <http://datatracker.ietf.org/doc/html/rfc9562 RFC 9562> for the specification.
-}
module Data.UUID.Types
    ( -- * The 'UUID' Type
      UUID
      -- * Nil UUID
    , nil
    , null
      -- * Max UUID
    , max
    , isMax
      -- * Textual Representation
    , toString
    , fromString
    , toText
    , fromText
    , toASCIIBytes
    , fromASCIIBytes
    , toLazyASCIIBytes
    , fromLazyASCIIBytes
      -- * Binary Representation
    , toByteString
    , fromByteString
      -- * Integer Representation
    , toWords
    , fromWords
    , toWords64
    , fromWords64
    ) where

import           Data.UUID.Types.Internal
import           Prelude                  ()

-- Everything is really implemented in Data.UUID.Types.Internal, but I
-- don't want to export the constructors out of the package.
