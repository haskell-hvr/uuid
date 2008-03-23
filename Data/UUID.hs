{-# INCLUDE <uuid/uuid.h> #-}
{-# INCLUDE <string.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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
-- Haskell bindings to /libuuid/.
-- The library /libuuid/ is available as a part of e2fsprogs:
-- <http://e2fsprogs.sourceforge.net/>.
--
-- This library is useful for creating, comparing, parsing and
-- printing Universally Unique Identifiers.
-- See <http://en.wikipedia.org/wiki/UUID> for the general idea.

module Data.UUID(UUID
                ,fromString
                ,toString
                ,toStringUpper
                ,toStringLower
                ,generate
                ,generateRandom
                ,generateTime
                ,null
                )
where

import Foreign.C.String
import Foreign.C
import Foreign.ForeignPtr
import Foreign

import Data.Typeable
import Data.Generics.Basics

import Prelude hiding (null)

import Data.UUID.Internal

instance Eq UUID where
    a == b = compare a b == EQ

instance Ord UUID where
    compare (U fp1) (U fp2) = unsafePerformIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        case c_compare p1 p2 of
           0     -> return EQ
           n|n<0 -> return LT
            |n>0 -> return GT

instance Show UUID where
    show = toString

instance Read UUID where
    readsPrec _ str = case fromString (take 36 str) of
      Nothing -> []
      Just u  -> [(u,drop 36 str)]

instance Typeable UUID where
    typeOf _ = mkTyConApp (mkTyCon "Data.UUID.UUID") []

instance Storable UUID where
    sizeOf _ = (16 *) $ alignment (undefined :: CChar)
    alignment _ = alignment (undefined :: CChar)

    peek psource = do
      fp <- mallocForeignPtrArray 16
      withForeignPtr fp $ \pdest ->
          memcpy pdest psource $ fromIntegral $ sizeOf (undefined :: UUID)
      return $ U fp

    poke pdest (U fp) = withForeignPtr fp $ \psource ->
          memcpy pdest psource $ fromIntegral $ sizeOf (undefined :: UUID)


-- My goal with this instance was to make it work just enough to do what
-- I want when used with the HStringTemplate library.
instance Data UUID where
    toConstr uu  = mkConstr uuidType (show uu) [] (error "fixity")
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = uuidType

uuidType =  mkNorepType "Data.UUID.UUID"


-- |Creates a new 'UUID'.  If \/dev\/urandom is available, it will be used.
-- Otherwise a UUID will be generated based on the current time and the
-- hardware MAC address, if available.
generate :: IO UUID
generate = generateWrap c_generate

-- |Create a new 'UUID'.  If \/dev\/urandom is available, it will be used.
-- Otherwise a psuedorandom generator will be used.
generateRandom :: IO UUID
generateRandom = generateWrap c_generate_random

-- |Create a new 'UUID'.  The UUID will be  generated based on the
-- current time and the hardware MAC address, if available.
generateTime :: IO UUID
generateTime = generateWrap c_generate_time

generateWrap :: (C_UUID -> IO ()) -> IO UUID
generateWrap gen = do
  fp <- mallocForeignPtrArray 16
  withForeignPtr fp $ \p -> gen p
  return $ U fp

-- |Returns 'True' if the passed-in 'UUID' is the null UUID.
null :: UUID -> Bool
null (U fp) = unsafePerformIO $
              withForeignPtr fp $ \p ->
              return $ c_null p == 1

-- |If the passed in 'String' can be parsed as a 'UUID', it will be.
-- The hyphens may not be omitted.
-- Example:
--
-- @
--  fromString \"c2cc10e1-57d6-4b6f-9899-38d972112d8c\"
-- @
--
-- Hex digits may be upper or lower-case.
fromString :: String -> Maybe UUID
fromString s = unsafePerformIO $ do
  fp <- mallocForeignPtrArray 16
  res <- withCAString s $ \chars ->
      withForeignPtr fp $ \p ->
      c_read chars p
  case res of
    0 -> return . Just $ U fp
    _ -> return Nothing

-- |Returns a 'String' representation of the passed in 'UUID'.
-- Hex digits occuring in the output will be either upper or
-- lower-case depending on system defaults and locale.
toString :: UUID -> String
toString = toStringWrap c_show

-- |Returns a 'String' representation of the passed in 'UUID'.
-- Hex digits occuring in the output will be lower-case.
toStringLower :: UUID -> String
toStringLower = toStringWrap c_show_lower
  
-- |Returns a 'String' representation of the passed in 'UUID'.
-- Hex digits occuring in the output will be upper-case.
toStringUpper :: UUID -> String
toStringUpper = toStringWrap c_show_upper

-- |The passed in function /f/ is given a the raw UUID data
-- and a 37-byte buffer to fill with the textual representation
-- of the UUID.  The buffer is expected to contain a null-terminator
-- after /f/ is finished.
toStringWrap :: (C_UUID -> CString -> IO ()) -> UUID -> String
toStringWrap f (U fp) = unsafePerformIO $
  allocaBytes 37 $ \chars -> do
  withForeignPtr fp $ \p -> f p chars
  peekCAString chars

-- FFI calls to do the work

type C_UUID = Ptr CChar

-- comparing UUIDs
foreign import ccall unsafe "uuid_compare"
  c_compare :: C_UUID -> C_UUID -> CInt

-- making random UUIDs
foreign import ccall unsafe "uuid_generate"
  c_generate :: C_UUID -> IO ()

foreign import ccall unsafe "uuid_generate_time"
  c_generate_time :: C_UUID -> IO ()

foreign import ccall unsafe "uuid_generate_random"
  c_generate_random :: C_UUID -> IO ()

-- Null check
foreign import ccall unsafe "uuid_is_null"
  c_null :: C_UUID -> CInt

-- Parsing
foreign import ccall unsafe "uuid_parse"
  c_read :: CString -> C_UUID ->IO CInt

-- Showing

foreign import ccall unsafe "uuid_unparse"
  c_show :: C_UUID -> CString -> IO ()

foreign import ccall unsafe "uuid_unparse_lower"
  c_show_lower :: C_UUID -> CString -> IO ()

foreign import ccall unsafe "uuid_unparse_upper"
  c_show_upper :: C_UUID -> CString -> IO ()


-- Queries

foreign import ccall unsafe "uuid_type"
  c_type :: C_UUID -> CInt

foreign import ccall unsafe "uuid_variant"
  c_variant :: C_UUID -> CInt

-- Other

foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a -> Ptr b -> CSize -> IO ()
