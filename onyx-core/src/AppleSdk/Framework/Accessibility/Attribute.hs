{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module AppleSdk.Framework.Accessibility.Attribute
  ( AXAttribute(..)
  , setAttribute
  , attributeValue
  , isAttributeSettable
  , windowRole
  , windowSubrole
  ) where

import Prelude hiding (String)
import Control.Monad.Managed
import Foreign hiding (with)
import Foreign.C.Types (CInt(..), CBool(..))
import AppleSdk.Framework.Accessibility.Error
import AppleSdk.Framework.Accessibility.UIElement
import AppleSdk.Framework.Accessibility.Value
import AppleSdk.Framework.CoreFoundation
import AppleSdk.Framework.Types (Iso(..), resultAction, action)

foreign import ccall ax_windows_attribute :: IO CFStringRef
foreign import ccall ax_focused_window_attribute :: IO CFStringRef
foreign import ccall ax_main_attribute :: IO CFStringRef
foreign import ccall ax_focused_attribute :: IO CFStringRef
foreign import ccall ax_title_attribute :: IO CFStringRef
foreign import ccall ax_role_attribute :: IO CFStringRef
foreign import ccall ax_subrole_attribute :: IO CFStringRef
foreign import ccall ax_position_attribute :: IO CFStringRef
foreign import ccall ax_size_attribute :: IO CFStringRef
foreign import ccall ax_close_button_attribute :: IO CFStringRef
foreign import ccall ax_fullscreen_attribute :: IO CFStringRef
foreign import ccall ax_minimized_attribute :: IO CFStringRef

data TyAttribute
  = TyWindowsAttribute
  | TyFocusedWindowAttribute
  | TyMainAttribute
  | TyFocusedAttribute
  | TyTitleAttribute
  | TyRoleAttribute
  | TySubroleAttribute
  | TyPositionAttribute
  | TySizeAttribute
  | TyCloseButtonAttribute
  | TyFullscreenAttribute
  | TyMinimizedAttribute
  deriving Eq

data AXAttribute (a :: TyAttribute) :: * where
  AXWindowsAttribute :: AXAttribute TyWindowsAttribute
  AXFocusedWindowAttribute :: AXAttribute TyFocusedWindowAttribute
  AXMainAttribute :: AXAttribute TyMainAttribute
  AXFocusedAttribute :: AXAttribute TyFocusedAttribute
  AXTitleAttribute :: AXAttribute TyTitleAttribute
  AXRoleAttribute :: AXAttribute TyRoleAttribute
  AXSubroleAttribute :: AXAttribute TySubroleAttribute
  AXPositionAttribute :: AXAttribute TyPositionAttribute
  AXSizeAttribute :: AXAttribute TySizeAttribute
  AXCloseButtonAttribute :: AXAttribute TyCloseButtonAttribute
  AXFullscreenAttribute :: AXAttribute TyFullscreenAttribute
  AXMinimizedAttribute :: AXAttribute TyMinimizedAttribute

type family AttrResult (a :: TyAttribute) :: * where
  AttrResult TyWindowsAttribute = Array UIElement
  AttrResult TyFocusedWindowAttribute = UIElement
  AttrResult TyMainAttribute = Boolean
  AttrResult TyFocusedAttribute = Boolean
  AttrResult TyPositionAttribute = Value
  AttrResult TySizeAttribute = Value
  AttrResult TyCloseButtonAttribute = UIElement
  AttrResult TyTitleAttribute = String
  AttrResult TyRoleAttribute = String
  AttrResult TySubroleAttribute = String
  AttrResult TyMinimizedAttribute = Boolean
  AttrResult TyFullscreenAttribute = Boolean

attributeString :: AXAttribute a -> IO CFStringRef
attributeString = \case
  AXWindowsAttribute -> ax_windows_attribute
  AXFocusedWindowAttribute -> ax_focused_window_attribute
  AXMainAttribute -> ax_main_attribute
  AXFocusedAttribute -> ax_focused_attribute
  AXTitleAttribute -> ax_title_attribute
  AXRoleAttribute -> ax_role_attribute
  AXSubroleAttribute -> ax_subrole_attribute
  AXPositionAttribute -> ax_position_attribute
  AXSizeAttribute -> ax_size_attribute
  AXCloseButtonAttribute -> ax_close_button_attribute
  AXFullscreenAttribute -> ax_fullscreen_attribute
  AXMinimizedAttribute -> ax_minimized_attribute

foreign import ccall "Carbon/Carbon.h AXUIElementSetAttributeValue"
  ax_ui_element_set_attribute_value
  :: AXUIElementRef -> CFStringRef -> CFTypeRef -> IO ForeignAXError

axUIElementSetAttributeValue
  :: CoreFoundationType a
  => AXUIElementRef -> AXAttribute attr -> Ptr a -> IO AXResult
axUIElementSetAttributeValue ref a value = do
  str <- attributeString a
  toAXResult <$> ax_ui_element_set_attribute_value ref str (toCFType value)

setAttribute :: CFObject obj => UIElement -> AXAttribute a -> obj -> AX ()
setAttribute el at ob = resultAction $ flip with pure $ do
  el' <- managed $ withCFPtr el
  p   <- managed $ withCFPtr ob
  liftIO $ axUIElementSetAttributeValue el' at p

foreign import ccall "Carbon/Carbon.h AXUIElementCopyAttributeValue"
  ax_ui_element_copy_attribute_value
  :: AXUIElementRef -> CFStringRef -> Ptr CFTypeRef -> IO ForeignAXError

axUIElementCopyAttributeValue
  :: AXUIElementRef -> AXAttribute a -> Ptr CFTypeRef -> IO AXResult
axUIElementCopyAttributeValue el a outptr = do
  str <- attributeString a
  fmap toAXResult (ax_ui_element_copy_attribute_value el str outptr)

attr :: AXAttribute a -> CFTypeRef -> IO (AttrResult a)
attr AXWindowsAttribute = manageCFObj . fromCFType
attr AXFocusedWindowAttribute = manageCFObj . fromCFType
attr AXMainAttribute = manageCFObj . fromCFType
attr AXFocusedAttribute = manageCFObj . fromCFType
attr AXTitleAttribute = manageCFObj . fromCFType
attr AXRoleAttribute = manageCFObj . fromCFType
attr AXSubroleAttribute = manageCFObj . fromCFType
attr AXPositionAttribute = manageCFObj . fromCFType
attr AXSizeAttribute = manageCFObj . fromCFType
attr AXCloseButtonAttribute = manageCFObj . fromCFType
attr AXFullscreenAttribute = manageCFObj . fromCFType
attr AXMinimizedAttribute = manageCFObj . fromCFType

attributeValue :: UIElement -> AXAttribute a -> AX (AttrResult a)
attributeValue uiel str = do
  eith <- action $ withCFPtr uiel $ \el -> alloca $ \p -> do
    res <- axUIElementCopyAttributeValue el str p
    case res of
      Err e -> pure (Left e)
      Ok -> Right <$> (peek p >>= attr str)
  pure eith

foreign import ccall unsafe "Carbon/Carbon.h AXUIElementIsAttributeSettable"
  ax_ui_element_is_attribute_settable
  :: AXUIElementRef -> CFStringRef -> Ptr CBool -> IO ForeignAXError

axUIElementIsAttributeSettable
  :: AXUIElementRef -> AXAttribute a -> IO (Either AXError CBool)
axUIElementIsAttributeSettable ref a = do
  str <- attributeString a
  alloca $ \p -> do
    err <- poke p (Foreign.fromBool False) >>
      ax_ui_element_is_attribute_settable ref str p
    res <- peek p
    pure $ isoTo (toAXResult err) >> pure res 

isAttributeSettable :: UIElement -> AXAttribute a -> AX Bool
isAttributeSettable el =
  fmap Foreign.toBool . action . withCFPtr el .  -- was: axErrorIO
  flip axUIElementIsAttributeSettable

foreign import ccall unsafe "ax_window_role" axWindowRole :: CFStringRef
foreign import ccall unsafe "ax_standard_window_subrole"
  axStandardWindowSubrole :: CFStringRef

windowRole :: IO String
windowRole = fmap fromCFPtr (newForeignPtr_ axWindowRole)

windowSubrole :: IO String
windowSubrole = fmap fromCFPtr (newForeignPtr_ axStandardWindowSubrole)
