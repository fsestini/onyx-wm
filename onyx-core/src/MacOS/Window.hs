-- | MacOS window management types and functions.

module MacOS.Window
  ( -- Window
  -- , 
    WindowID
  , Window

  -- -- * Actions
  -- , focusWindow
  -- , closeWindow
  -- , createWindow

  -- * Query
  -- , windowID
  , windowTitle
  , windowID
  , sameWID
  -- , focusedWindowInApp
  -- , focusedWindowIDInApp
  -- , windowParent
  -- , getValidWindows
  , appWindows

  -- * Position/size attributes
  -- , setWindowPosition
  -- , setWindowSize
  , setWindowRect
  -- , windowPosition
  -- , windowSize
  , windowRect
  , isWindowMovable
  , isWindowResizable

  -- * Visibility attributes
  , isWindowMinimized
  , setNativeFullscreen
  , nativeFullscreenState

  -- -- * Notifications
  -- , registerWindowNotifications
  -- , removeWindowNotifications

  -- * Other attributes
--  , isValidWindow
  , isWindowStandard

  , windowParent
  , focusedWindowInApp
  , focusWindow
  , closeWindow
  ) where

import Onyx.Prelude

import AppleSdk
       (WindowID, Rect(..), ioFrom, AXAttribute(..), attributeValue,
        setAttribute, getElementWindow, booleanTrue, booleanFalse,
        setFrontProcessFrontWindowOnly, performAction, AXAction(..),
        isAttributeSettable, objEquals, windowRole, windowSubrole)
import MacOS.Internal.Window
import MacOS.Internal.App
import MacOS.App (focusedApp)
import MacOS.Internal.Type

import Control.Monad.Loops (whileM_)
import ExceptExtra (MonadError(..))

windowID :: Window -> WindowID
windowID = _windowID

windowParent :: Window -> App
windowParent = _windowParent

closeWindow :: (MonadMac m, MonadError MacError m) => Window -> m ()
closeWindow w = injAction AXError $
  attributeValue (_windowElement w) AXCloseButtonAttribute >>=
  flip performAction AXPressAction

-- | Returns whether a window can be moved.
isWindowMovable :: (MonadMac m, MonadError MacError m) => Window -> m Bool
isWindowMovable =
  injAction AXError . flip isAttributeSettable AXPositionAttribute . _windowElement

-- | Returns whether a window can be resized.
isWindowResizable :: (MonadMac m, MonadError MacError m) => Window -> m Bool
isWindowResizable =
  injAction AXError . flip isAttributeSettable AXSizeAttribute . _windowElement

-- | Returns whether a window is minimized.
isWindowMinimized :: (MonadMac m, MonadError MacError m) => Window -> m Bool
isWindowMinimized w =
  injAction AXError . join . fmap (liftIO . ioFrom) $
    attributeValue (_windowElement w) AXMinimizedAttribute

-- | A window is standard whenever the associated Accessibility API element's
-- role and subrole correspond, respectively, to 'windowRole' and
-- 'windowSubrole'.
isWindowStandard :: (MonadMac m, MonadError MacError m) => Window -> m Bool
isWindowStandard w = injAction AXError $ do
  r <- attributeValue (_windowElement w) AXRoleAttribute
  sr <- attributeValue (_windowElement w) AXSubroleAttribute
  liftIO $
    (&&) <$> (windowRole >>= objEquals r) <*> (windowSubrole >>= objEquals sr)

-- | Title of the window.
--
-- Since an window's title can change over time, the function retrieves it
-- from scratch at each invocation. Hence the side effect.
windowTitle :: (MonadMac m, MonadError MacError m) => Window -> m String
windowTitle w = injAction AXError $
  attributeValue (_windowElement w) AXTitleAttribute >>= liftIO . ioFrom

windowRect :: (MonadMac m, MonadError MacError m) => Window -> m Rect
windowRect w =
  join . fmap (maybe (throwError UnspecifiedError) pure) . injAction AXError $ do
    pt <- windowPosition w
    sz <- windowSize w
    pure $ Rect <$> pt <*> sz

setWindowRect :: (MonadMac m, MonadError MacError m) => Window -> Rect -> m ()
setWindowRect w Rect {..} =
  injAction AXError
    (setWindowSize w size >> setWindowPosition w origin >> setWindowSize w size)

setNativeFullscreen :: (MonadMac m, MonadError MacError m) => Window -> Bool -> m ()
setNativeFullscreen w st =
  injAction AXError $ (liftIO $ if st then booleanTrue else booleanFalse) >>=
    setAttribute (_windowElement w) AXFullscreenAttribute

-- | Returns whether the window is in native fullscreen state.
nativeFullscreenState :: (MonadMac m, MonadError MacError m) => Window -> m Bool
nativeFullscreenState =
  liftIO . ioFrom <=<
  injAction AXError . flip attributeValue AXFullscreenAttribute . _windowElement

-- | Returns the 'Window' of the applications window that is currently focused.
focusedWindowInApp :: (MonadMac m, MonadError MacError m) => App -> m Window
focusedWindowInApp a = do
  we <- injAction AXError . getFocusedWindowElement . appElement $ a
  wid <- injAction AXError (getElementWindow we)
  findByWID wid

focusWindow :: (MonadMac m, MonadError MacError m) => Window -> m ()
focusWindow w = do
  t <- windowTitle w
  liftIO (putStrLn ("focusing window: " ++ t))
  
  whileM_ (fmap (not . sameWID w) currentlyFoc) $ do
    liftIO (setFrontProcessFrontWindowOnly (_appPID . _windowParent $ w))
    injAction AXError $ do
      bt >>= setAttribute (_windowElement w) AXMainAttribute
      bt >>= setAttribute (_windowElement w) AXFocusedAttribute
  liftIO (putStrLn "done.")
  
  where
    bt = liftIO booleanTrue
    currentlyFoc = focusedApp >>= focusedWindowInApp

appWindows :: MonadMac m => App -> m [Window]
appWindows a = fmap (filter ((== (_appPID a)) . _appPID . _windowParent)) wins
