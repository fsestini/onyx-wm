module AppleSdk.Framework.CoreFoundation.MachPort
  ( CFMachPortRef
  , MachPort
  , eventTapIsEnabled
  , enableEventTap
  ) where

import Foreign
import Foreign.C.Types (CBool(..))
import AppleSdk.Framework.CoreFoundation.Object

data CFMachPort
type CFMachPortRef = Ptr CFMachPort
instance CoreFoundationType CFMachPort where
newtype MachPort = MachPort (ForeignPtr CFMachPort) deriving CFObject

foreign import ccall unsafe "CGEventTapIsEnabled"
  cgEventTapIsEnabled :: CFMachPortRef -> IO CBool

eventTapIsEnabled :: MachPort -> IO Bool
eventTapIsEnabled = fmap toBool . flip withCFPtr cgEventTapIsEnabled

foreign import ccall unsafe "CGEventTapEnable"
  cgEventTapEnable :: CFMachPortRef -> CBool -> IO ()

enableEventTap :: MachPort -> Bool -> IO ()
enableEventTap mp b = withCFPtr mp $ flip cgEventTapEnable (fromBool b)
