module AppleSdk.Framework.CoreFoundation.UUID
  ( CFUUIDRef
  , UUID
  ) where

import Foreign
import AppleSdk.Framework.CoreFoundation.Object

data CFUUID
type CFUUIDRef = Ptr CFUUID
instance CoreFoundationType CFUUID where
newtype UUID = UUID (ForeignPtr CFUUID) deriving CFObject
