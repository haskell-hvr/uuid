-- |
-- Module      : Data.UUID.Internal
-- Copyright   : (c) 2008 Antoine Latter
--
-- License     : BSD-style
--
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- If the public interface in "Data.UUID" doesn't give you
-- the flexibility you need, you should be able to find
-- something here.

module Data.UUID.Internal(UUID(..)
                         ,toBytes
                         ,fromBytes
                         ,unsafeFromBytes
                         ,toForeignPtr
                         ,fromForeignPtr
                         ) where

import Foreign.ForeignPtr
import Foreign.C
import Foreign

-- |The UUID type.  Represents 128-bits of identification.
newtype UUID = U (ForeignPtr CChar)

-- |Returns the passed in UUID as a list of 16 bytes.
toBytes :: UUID -> [Word8]
toBytes (U fp) = unsafePerformIO $ withForeignPtr fp $ \p ->
                 peekArray 16 (castPtr p)

-- |Creates a UUID out of a list of bytes.
-- Will throw an error if the list is not of length 16.
fromBytes :: [Word8] -> UUID
fromBytes  xs = if length xs == 16
 then unsafeFromBytes xs
 else error "Data.UUID.Internal.fromBytes: passed in list of bytes must be of length 16."

-- |Creates a UUID out of a list of bytes.
-- Does not perform a length check.
-- Behavior is undefined for lists not of length 16.
unsafeFromBytes :: [Word8] -> UUID
unsafeFromBytes xs = U $ castForeignPtr $ unsafePerformIO $ do
   p <- mallocForeignPtrBytes 16
   withForeignPtr p $ flip pokeArray xs
   return p

-- |Given a UUID, returns a pointer to the 16 bytes
-- of memory that make up the UUID.
toForeignPtr :: UUID -> ForeignPtr CChar
toForeignPtr (U p) = p

-- |The passed in pointer is treated as if it were a pointer
-- to 16 bytes of memory.  You're in trouble if it isn't.
fromForeignPtr :: ForeignPtr CChar -> UUID
fromForeignPtr = U
