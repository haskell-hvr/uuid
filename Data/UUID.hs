{- |
Module      : Data.UUID
Copyright   : (c) 2008,2012 Antoine Latter

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable


This library is useful for comparing, parsing and
printing Universally Unique Identifiers.
See <http://en.wikipedia.org/wiki/UUID> for the general idea.
See <http://tools.ietf.org/html/rfc4122> for the specification.

* Random UUIDs may be generated using 'Data.UUID.V4.nextRandom' or
your favorite instance of 'System.Random.Random'.

* We have an implementation of generating a UUID from the hardware
MAC address and current system time in "Data.UUID.V1".

* For name-based generation of UUIDs using SHA-1 hashing see
"Data.UUID.V5".
-}

module Data.UUID(UUID
                ,toString
                ,fromString
                ,toByteString
                ,fromByteString
                ,toWords
                ,fromWords
                ,null
                ,nil
                ,fromASCIIBytes
                ,toASCIIBytes
                ,fromLazyASCIIBytes
                ,toLazyASCIIBytes
                ) where

import Prelude () -- we need to hide Prelude.null
import Data.UUID.Internal

-- Everything is really implemented in Data.UUID.Internal,
-- but I don't want to export the constructors out of the
-- package.
