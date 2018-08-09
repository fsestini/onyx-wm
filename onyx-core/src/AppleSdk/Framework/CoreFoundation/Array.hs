module AppleSdk.Framework.CoreFoundation.Array
  ( CFArrayRef
  , CFIndex
  , Array
  , createArray
  , createArray'
  , arrayValues
  ) where

import AppleSdk.Framework.CoreFoundation.Object
import AppleSdk.Framework.CoreFoundation.Allocator
import Foreign.Ptr
import Foreign hiding (with)
import Foreign.C.Types (CInt(..))
import Control.Monad
import Control.Monad.Managed
import Data.Foldable

data CFArray
type CFArrayRef = Ptr CFArray
instance CoreFoundationType CFArray
newtype Array a = Array (ForeignPtr CFArray) deriving (CFObject)

type CFIndex = CInt

foreign import ccall unsafe "Carbon/Carbon.h CFArrayGetValueAtIndex"
  cf_array_get_value_at_index :: CFArrayRef -> CFIndex -> IO (Ptr ())

foreign import ccall unsafe "Carbon/Carbon.h CFArrayGetCount"
  cf_array_get_count :: CFArrayRef -> IO CFIndex

getCFArrayValues :: CFArrayRef -> IO [Ptr a]
getCFArrayValues arr = do
  n <- cf_array_get_count arr
  ptrs <- forM [0.. (n-1)] (cf_array_get_value_at_index arr)
  pure $ fmap castPtr ptrs

arrayValues :: CFObject a => Array a -> IO [a]
arrayValues arr = withCFPtr arr $ \cfarr -> do
  vals <- getCFArrayValues cfarr
  forM_ vals cfRetain
  ptrs <- forM vals (newForeignPtr cfReleasePtr)
  pure $ fmap fromCFPtr ptrs

--------------------------------------------------------------------------------
-- Array creation

data ArrayCallbacks

foreign import ccall cfCallbs :: Ptr ArrayCallbacks

foreign import ccall "CFArrayCreate"
  cfArrayCreate :: CFAllocatorRef -> Ptr (Ptr ()) -> CFIndex
                -> Ptr ArrayCallbacks -> IO CFArrayRef

-- | Create an array from a denumerable collection of Core Foundation objects,
-- using the specified allocator.
createArray :: (Foldable f, CFObject obj) => Allocator -> f obj -> IO (Array obj)
createArray allo objs = flip with pure $ do
  allo' <- managed (withCFPtr allo)
  objs' <- managed (withMany withCFPtr (toList objs))
  liftIO $ do
    arr <- newArray objs'
    cfArrayCreate allo' (castPtr arr) (fromIntegral (length objs')) cfCallbs
      >>= manageCFObj

-- | Create an array from a denumerable collection of Core Foundation objects,
-- using the default allocator.
createArray' :: (Foldable f, CFObject obj) => f obj -> IO (Array obj)
createArray' objs = nullAllocator >>= flip createArray objs
