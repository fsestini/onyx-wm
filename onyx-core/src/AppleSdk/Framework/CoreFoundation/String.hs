{-# LANGUAGE TypeSynonymInstances #-}

module AppleSdk.Framework.CoreFoundation.String
  ( StringEncoding
  , CFStringRef
  , AppleSdk.Framework.CoreFoundation.String.String
  , fromCString
  , toCString
  ) where

import AppleSdk.Framework.Types
import AppleSdk.Framework.CoreFoundation.Allocator
import AppleSdk.Framework.CoreFoundation.Object
import AppleSdk.Framework.CoreFoundation.Array
import Foreign
import Foreign.C.String
import Foreign.C.Types (CBool(..), CInt(..))
import Control.Monad
import Data.Bool (bool)

type StringEncoding = Word32

foreign import ccall unsafe "cfStringEncodingASCII"
  stringEncodingASCII :: StringEncoding
foreign import ccall unsafe "uif8enc" stringEncodingUTF8 :: StringEncoding

data CFString
type CFStringRef = Ptr CFString
instance CoreFoundationType CFString
newtype String = String (ForeignPtr CFString) deriving (Show, CFObject)

type ThisString = AppleSdk.Framework.CoreFoundation.String.String

foreign import ccall unsafe "CFStringGetCString"
  cfStringGetCString :: CFStringRef -> CString -> CFIndex -> StringEncoding -> IO CBool

toString :: ThisString -> IO Prelude.String
toString = toCString >=> peekCString

foreign import ccall "CFStringCreateWithCString" cfStringCreateWithCString
  :: CFAllocatorRef -> Ptr a -> StringEncoding -> IO CFStringRef

foreign import ccall "CFStringGetLength"
  cfStringGetLength :: CFStringRef -> IO CFIndex

foreign import ccall "CFStringGetMaximumSizeForEncoding"
  cfStringGetMaximumSizeForEncoding :: CFIndex -> StringEncoding -> IO CFIndex

toCString :: ThisString -> IO CString
toCString str = withCFPtr str $ \s -> do
  len <- cfStringGetLength s
  bytes <- cfStringGetMaximumSizeForEncoding len stringEncodingUTF8
  cstr <- mallocArray (fromIntegral bytes + 1)
  res <- cfStringGetCString s cstr (bytes + 1) stringEncodingUTF8
  bool (free cstr >> newCString "") (pure cstr) (toBool res)

fromCString :: Allocator -> CString -> StringEncoding -> IO ThisString
fromCString allo cstr enc = withCFPtr allo $ \pallo ->
  cfStringCreateWithCString pallo cstr enc >>= manageCFObj

fromString :: Prelude.String -> IO ThisString
fromString str = do
  allo <- nullAllocator
  cstr <- newCString str
  fromCString allo cstr stringEncodingASCII

instance IOIso Prelude.String ThisString where
  ioTo = fromString
  ioFrom = toString

-- type UniCharCount = Word32
-- type UniChar = Word16

-- foreign import ccall "CFStringCreateWithCharacters"
--   cfStringCreateWithCharacters :: CFAllocatorRef -> Ptr UniChar -> CFIndex
--     -> IO CFStringRef

-- createStringWithCharacters :: Allocator -> Ptr UniChar -> CFIndex -> IO ThisString
-- createStringWithCharacters allo p idx = withCFPtr allo $ \allo' ->
--   cfStringCreateWithCharacters allo' p idx >>= manageCFObj
