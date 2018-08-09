module AppleSdk.Framework.Accessibility.UIElement
  ( UIElement
  , AXUIElementRef
  , AppElement
  , WindowElement
  , createApplicationUIElement
  , getElementWindow
  , setMessagingTimeout
  , systemWideUIElement
  ) where

import AppleSdk.Framework.Accessibility.Error
import AppleSdk.Framework.CoreFoundation.Object
import AppleSdk.Framework.Types
import Foreign
import Foreign.C.Types (CInt(..))
import Control.Monad

data AXUIElement
type AXUIElementRef = Ptr AXUIElement

-- | This type represents a reference to an accessible user interface element.
newtype UIElement = UIElement { getUIElement :: ForeignPtr AXUIElement }

instance CoreFoundationType AXUIElement
instance CFObject UIElement where
  type CFT UIElement = AXUIElement
  toCFPtr = getUIElement
  fromCFPtr = UIElement

type AppElement = UIElement
type WindowElement = UIElement

foreign import ccall unsafe "AXUIElementCreateApplication"
  ax_ui_element_create_application :: PID -> IO AXUIElementRef

-- | Create a UIElement from a PID, representing the application associated to
-- it.
createApplicationUIElement :: PID -> IO UIElement
createApplicationUIElement = ax_ui_element_create_application >=> manageCFObj

foreign import ccall unsafe "_AXUIElementGetWindow"
  axUIElementGetWindow :: AXUIElementRef -> Ptr WindowID -> IO ForeignAXError

-- | Retrieve the WindowID of the window associated to the UI element.
getElementWindow :: UIElement -> AX WindowID
getElementWindow el = action . withCFPtr el $ \elptr -> alloca $ \widptr -> do
  err <- axUIElementGetWindow elptr widptr
  let res = toAXResult err
  case res of
    Ok -> Right <$> peek widptr
    Err e -> pure $ Left e

foreign import ccall unsafe "AXUIElementSetMessagingTimeout"
  axUIElementSetMessagingTimeout :: AXUIElementRef -> Float -> IO ForeignAXError

setMessagingTimeout :: UIElement -> Float -> AX ()
setMessagingTimeout el f = resultAction . fmap toAXResult $
  withCFPtr el (flip axUIElementSetMessagingTimeout f)

foreign import ccall unsafe "AXUIElementCreateSystemWide"
  axUIElementCreateSystemWide :: IO AXUIElementRef

systemWideUIElement :: IO UIElement
systemWideUIElement = axUIElementCreateSystemWide >>= manageCFObj
