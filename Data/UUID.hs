{-# INCLUDE "cbits.h" #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}

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
import Foreign

import Data.Char

import Prelude hiding (null)

#if defined(__GLASGOW_HASKELL__)
import GHC.Base (unsafeChr)
#endif

newtype UUID = U C_UUID

type C_UUID = Ptr ()

instance Eq UUID where
    (U a) == (U b) = c_compare a b == 0

instance Ord UUID where
    compare (U p1) (U p2) =
        case c_compare p1 p2 of
           0 -> EQ
           n|n<0 -> LT
            |n>0 -> GT

instance Show UUID where
    show = toString

generate :: IO UUID
generate = do
  p <- mallocBytes 16
  c_generate p
  return $ U p

generateRandom :: IO UUID
generateRandom = do
  p <- mallocBytes 16
  c_generate_random p
  return $ U p

generateTime :: IO UUID
generateTime = do 
  p <- mallocBytes 16
  c_generate_time p
  return $ U p

null :: UUID -> Bool
null (U p) = c_null p /= 0

fromString :: String -> Maybe UUID
fromString s = unsafePerformIO $ do
  p <- mallocBytes 16
  chars <- mallocBytes 37
  pack 0 s chars
  res <- c_read p (castPtr chars)
  case res of
    0 -> return . Just $ U p
    _ -> return Nothing

pack :: Int -> String -> Ptr Word8 -> IO ()
pack 37 _ p = pokeByteOff p 37 (0 :: Word8)
pack n (c:cs) p = do
    pokeByteOff p n (c2w c)
    pack (n+1) cs p
pack n [] p = pokeByteOff p n (0 :: Word8)

unpack :: Ptr Word8 -> Int -> IO String
unpack p 37 = return []
unpack p n = do
  c <- peekByteOff p n
  case c of
    0 -> return []
    _ -> do
      cs <- unpack p (n+1)
      return $ (w2c c) : cs

toString :: UUID -> String
toString (U p) = unsafePerformIO $ do
  chars <- mallocBytes 37
  c_show p chars
  unpack (castPtr chars) 0

toStringLower :: UUID -> String
toStringLower (U p) = unsafePerformIO $ do
  chars <- mallocBytes 37
  c_show_lower p chars
  unpack (castPtr chars) 0

toStringUpper :: UUID -> String
toStringUpper (U p) = unsafePerformIO $ do
  chars <- mallocBytes 37
  c_show_upper p chars
  unpack (castPtr chars) 0

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
-- Shamelessly stolen from the 'bytestring' package.
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
-- Shamelessly stolen from the 'bytestring' package.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- comparing UUIDs
foreign import ccall unsafe "c_compare"
  c_compare :: C_UUID -> C_UUID -> CInt

-- making random UUIDs
foreign import ccall unsafe "c_generate"
  c_generate :: C_UUID -> IO ()

foreign import ccall unsafe "c_generate_time"
  c_generate_time :: C_UUID -> IO ()

foreign import ccall unsafe "c_generate_random"
  c_generate_random :: C_UUID -> IO ()

-- Null check
foreign import ccall unsafe "c_null"
  c_null :: C_UUID -> CInt


-- Parsing
foreign import ccall unsafe "c_read"
  c_read :: C_UUID -> CString ->IO CInt

-- Showing

foreign import ccall unsafe "c_show"
  c_show :: C_UUID -> CString -> IO ()

foreign import ccall unsafe "c_show_lower"
  c_show_lower :: C_UUID -> CString -> IO ()

foreign import ccall unsafe "c_show_upper"
  c_show_upper :: C_UUID -> CString -> IO ()


-- Queries

foreign import ccall unsafe "c_type"
  c_type :: C_UUID -> CInt

foreign import ccall unsafe "c_variant"
  c_variant :: C_UUID -> CInt
