module AppleSdk.Framework.CoreFoundation.Object where

import Control.Monad
import Control.Monad.Managed
import Foreign hiding (with)
import Foreign.C.Types (CBool(..))

class CoreFoundationType a where
  toCFType :: Ptr a -> Ptr CFType
  toCFType = castPtr
  fromCFType :: Ptr CFType -> Ptr a
  fromCFType = castPtr

instance CoreFoundationType CFType

class CoreFoundationType (CFT obj) => CFObject obj where
  type CFT obj
  toCFPtr :: obj -> ForeignPtr (CFT obj)
  fromCFPtr :: ForeignPtr (CFT obj) -> obj

instance CoreFoundationType a => CFObject (ForeignPtr a) where
  type CFT (ForeignPtr a) = a
  toCFPtr = id
  fromCFPtr = id

fromCFPtr' :: CFObject obj => Ptr (CFT obj) -> IO obj
fromCFPtr' = fmap fromCFPtr . newForeignPtr_

withCFPtr :: CFObject obj => obj -> (Ptr (CFT obj) -> IO b) -> IO b
withCFPtr obj = withForeignPtr (toCFPtr obj)

isNull :: CFObject obj => obj -> IO Bool
isNull = flip withCFPtr (pure . (== nullPtr))

manageCFObj :: CFObject obj => Ptr (CFT obj) -> IO obj
manageCFObj ptr = fmap fromCFPtr $ newForeignPtr cfReleasePtr ptr

retainManageCFObj :: CFObject obj => Ptr (CFT obj) -> IO obj
retainManageCFObj ref = cfRetain ref >> manageCFObj ref

data CFType
type CFTypeRef = Ptr CFType
newtype Object = Object { getObject :: ForeignPtr CFType }

type CFTypeID = Word32

instance CFObject Object where
  type CFT Object = CFType
  toCFPtr = getObject
  fromCFPtr = Object

foreign import ccall "Carbon/Carbon.h CFEqual"
  cf_equal :: CFTypeRef -> CFTypeRef -> IO CBool

cfEqual :: CoreFoundationType a => Ptr a -> Ptr a -> IO Bool
cfEqual x y = fmap toBool $ cf_equal (toCFType x) (toCFType y)

withManaged :: Managed a -> IO a
withManaged = flip with pure

objEquals :: CFObject obj => obj -> obj -> IO Bool
objEquals x y = join . withManaged $
  (liftM2 cfEqual) (managed $ withCFPtr x) (managed $ withCFPtr y)

foreign import ccall "Carbon/Carbon.h CFRetain"
  cf_retain :: CFTypeRef -> IO ()

cfRetain :: CoreFoundationType a => Ptr a -> IO ()
cfRetain x = cf_retain (toCFType x)

foreign import ccall "CFRelease"
  cfRelease :: CFTypeRef -> IO ()

foreign import ccall "&CFRelease"
  cfReleasePtr :: FunPtr (Ptr a -> IO ())

foreign import ccall "CFShow" cf_show :: CFTypeRef -> IO ()

cfShow :: CFObject obj => obj -> IO ()
cfShow obj = withCFPtr obj $ \p -> cf_show (toCFType p)

foreign import ccall "CFGetTypeID" cfGetTypeID :: CFTypeRef -> IO CFTypeID

getTypeID :: CFObject obj => obj -> IO CFTypeID
getTypeID = flip withCFPtr (cfGetTypeID . toCFType)
