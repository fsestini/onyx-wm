module AppleSdk.Framework.Accessibility.Action where

import Control.Monad ((>=>))
import AppleSdk.Framework.Accessibility.Error
import AppleSdk.Framework.Accessibility.UIElement
import AppleSdk.Framework.CoreFoundation
import AppleSdk.Framework.Types (resultAction)
import Foreign.C.Types (CInt(..))

data AXAction = AXPressAction

foreign import ccall unsafe "Carbon/Carbon.h AXUIElementPerformAction"
  ax_ui_element_perform_action
  :: AXUIElementRef -> CFStringRef -> IO ForeignAXError

foreign import ccall unsafe ax_press_action :: IO CFStringRef

actionString :: AXAction -> IO CFStringRef
actionString AXPressAction = ax_press_action

axUIElementPerformAction :: AXUIElementRef -> AXAction -> IO AXResult
axUIElementPerformAction ref =
  actionString >=> fmap toAXResult . ax_ui_element_perform_action ref

axUIElementPerformAction' :: AXUIElementRef -> AXAction -> AX ()
axUIElementPerformAction' ref = resultAction . axUIElementPerformAction ref

performAction :: UIElement -> AXAction -> AX ()
performAction el = resultAction . withCFPtr el . flip axUIElementPerformAction
