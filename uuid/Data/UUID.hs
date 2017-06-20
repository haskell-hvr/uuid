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

* Use 'Data.UUID.V4.nextRandom' to generate secure random UUIDs, and your
favorite instance of 'System.Random.Random' for faster but insecure
generation of UUIDs.

* We have an implementation of generating a UUID from the hardware
MAC address and current system time in "Data.UUID.V1".

* For name-based generation of UUIDs using SHA-1 hashing see
"Data.UUID.V5".
-}
module Data.UUID(UUID
                ,toString
                ,fromString
                ,toText
                ,fromText
                ,toASCIIBytes
                ,fromASCIIBytes
                ,toLazyASCIIBytes
                ,fromLazyASCIIBytes
                ,toByteString
                ,fromByteString
                ,toWords
                ,fromWords
                ,null
                ,nil
                ) where

import Prelude () -- we need to hide Prelude.null
import Data.UUID.Types

-- We use explicit re-exports of everything from Data.UUID.Types in
-- preference to just re-exporting the whole module. This is to avoid
-- unforeseen transitive API breakage if the Data.UUID.Types module
-- should change.
