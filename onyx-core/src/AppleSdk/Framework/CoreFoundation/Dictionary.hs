module AppleSdk.Framework.CoreFoundation.Dictionary
  ( Dictionary
  , CFDictionaryKeyCallBacks
  , CFDictionaryValueCallBacks
  , createDictionary
  , getDictValue
  , stringDictionaryKeyCallBacks
  , cfTypeDictionaryValueCallBacks
  , strKey
  ) where

import Control.Monad.Managed
import Foreign.Ptr
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array
import Foreign hiding (with)
import AppleSdk.Framework.Types
import AppleSdk.Framework.CoreFoundation.Object
import AppleSdk.Framework.CoreFoundation.Array
import AppleSdk.Framework.CoreFoundation.Allocator
import qualified AppleSdk.Framework.CoreFoundation.String as S

data CFDictionary
type CFDictionaryRef = Ptr CFDictionary
instance CoreFoundationType CFDictionary where
newtype Dictionary = Dictionary (ForeignPtr CFDictionary) deriving (CFObject)

foreign import ccall unsafe "cfCopyStringDictionaryKeyCallBacks"
  stringDictionaryKeyCallBacks :: IO (Ptr CFDictionaryKeyCallBacks)
foreign import ccall unsafe "cfTypeDictionaryValueCallBacks"
  cfTypeDictionaryValueCallBacks :: IO (Ptr CFDictionaryValueCallBacks)

data CFDictionaryKeyCallBacks
data CFDictionaryValueCallBacks

foreign import ccall unsafe "CFDictionaryCreate" cfDictionaryCreate
  :: CFAllocatorRef -> Ptr (Ptr a) -> Ptr (Ptr b) -> CFIndex
  -> Ptr CFDictionaryKeyCallBacks -> Ptr CFDictionaryValueCallBacks
  -> IO CFDictionaryRef

createDictionary
  :: CFObject obj => Allocator -> [S.String] -> [obj] -> IO Dictionary
createDictionary allo keys vals = flip with pure $ do
  allo' <- managed (withCFPtr allo)
  keys' <- managed (withMany withCFPtr keys)
  keys'' <- liftIO (newArray keys')
  vals' <- managed (withMany withCFPtr vals)
  vals'' <- liftIO (newArray vals')
  k1 <- liftIO stringDictionaryKeyCallBacks
  k2 <- liftIO cfTypeDictionaryValueCallBacks
  liftIO $
    cfDictionaryCreate allo' keys'' vals'' (fromIntegral (length keys)) k1 k2
      >>= manageCFObj

foreign import ccall unsafe "CFDictionaryGetValue"
  cfDictionaryGetValue :: CFDictionaryRef -> Ptr a -> IO (Ptr b)

getDictValue :: (CFObject key, CFObject val) => Dictionary -> key -> IO val
getDictValue dict key = flip with pure $ do
  d' <- managed (withCFPtr dict)
  k' <- managed (withCFPtr key)
  liftIO $ cfDictionaryGetValue d' k' >>= retainManageCFObj

strKey :: String -> IO S.String
strKey = ioTo

-- foreign import ccall azder :: CFDictionaryRef -> IO CFNumberRef

-- azder' :: Dictionary -> IO Number
-- azder' d = withCFPtr d azder >>= manageCFObj
