{-# LANGUAGE CPP #-}

-- stolen from GHC.ByteOrder
module ByteOrder where

#include "MachDeps.h"

-- | Byte ordering.
data ByteOrder = BigEndian | LittleEndian deriving (Eq, Ord, Bounded, Enum, Read, Show)

targetByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
targetByteOrder = BigEndian
#else
targetByteOrder = LittleEndian
#endif
