module AppleSdk.Framework.CoreFoundation.Allocator
  ( Allocator
  , CFAllocatorRef
  , nullAllocator
  ) where

import Foreign
import AppleSdk.Framework.CoreFoundation.Object

data CFAllocator
type CFAllocatorRef = Ptr CFAllocator
instance CoreFoundationType CFAllocator where
newtype Allocator = Allocator (ForeignPtr CFAllocator) deriving (CFObject)

nullAllocator :: IO Allocator
nullAllocator = fmap fromCFPtr $ newForeignPtr_ nullPtr
