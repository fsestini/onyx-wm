{-# LANGUAGE RecordWildCards #-}

module MacOS.Internal.Window where

import AppleSdk.Framework
import Control.Monad
import MacOS.Internal.App
import Foreign.StablePtr

data Window = Window
  { _windowParent :: App
  , _windowElement :: WindowElement
  , _windowID :: WindowID
  }

-- | Retrieves the accessibility elements representing the windows associated to
-- a given application reference.
getWindowElements :: AppElement -> AX [WindowElement]
getWindowElements app = attributeValue app AXWindowsAttribute >>= liftIO . arrayValues

createWindow :: App -> WindowElement -> AX Window
createWindow app ref =
  Window app ref <$> getElementWindow ref

-- | Return all windows of an application.
windows :: App -> AX [Window]
windows app@(App {..}) = getWindowElements appElement >>= mapM (createWindow app)

registerWindowNotifications :: App -> Window -> [UINotification] -> AX ()
registerWindowNotifications App {..} Window {..} notifs = do
  (liftIO $ newStablePtr _windowID) >>=
    forM_ notifs . flip (addNotification appObserver _windowElement)

removeWindowNotifications :: App -> Window -> [UINotification] -> AX ()
removeWindowNotifications App {..} Window {..} notifs =
  forM_ notifs (removeNotification appObserver _windowElement)

-- -- | Title of the window.
-- --
-- -- Since an window's title can change over time, the function retrieves it
-- -- from scratch at each invocation. Hence the side effect.
-- windowTitle :: Window -> AX Prelude.String
-- windowTitle Window {..} =
--   attributeValue _windowElement AXTitleAttribute >>= liftIO . ioFrom

-- | Returns the reference of the currently focused window for a given
-- application.
getFocusedWindowElement :: AppElement -> AX WindowElement
getFocusedWindowElement = flip attributeValue AXFocusedWindowAttribute

-- -- | Returns the applications window that is currently focused.
-- focusedWindowInApp :: App -> AX Window
-- focusedWindowInApp app@(App {..}) =
--   getFocusedWindowElement appElement >>= createWindow app

-- -- | Returns the 'WindowD' of the applications window that is currently focused.
-- focusedWindowIDInApp :: App -> AX WindowID
-- focusedWindowIDInApp = fmap _windowID . focusedWindowInApp

setWindowPosition :: Window -> Point -> AX ()
setWindowPosition Window {..} pt =
  (liftIO $ createCFPoint pt) >>= setAttribute _windowElement AXPositionAttribute

setWindowSize :: Window -> Size -> AX ()
setWindowSize Window {..} sz =
  (liftIO $ createCFSize sz) >>= setAttribute _windowElement AXSizeAttribute

-- | Returns the window position.
windowPosition :: Window -> AX (Maybe Point)
windowPosition w =
  attributeValue (_windowElement w) AXPositionAttribute >>= liftIO . valueGetPoint

-- | Returns the window size.
windowSize :: Window -> AX (Maybe Size)
windowSize w =
  attributeValue (_windowElement w) AXSizeAttribute >>= liftIO . valueGetSize

-- hasWID :: WindowID -> Window -> Bool
-- hasWID wanted Window {..} = wanted == _windowID

-- hasApp :: PID -> Window -> Bool
-- hasApp pid Window {..} = _windowParent == pid

sameWID :: Window -> Window -> Bool
sameWID w1 w2 = _windowID w1 == _windowID w2

-- | Returns whether the window is a valid window, where a /valid window/ is
-- defined to be one with non-zero ID.
isValidWindow :: Window -> Bool
isValidWindow Window {..} = _windowID /= 0

getValidWindows :: App -> AX [Window]
getValidWindows = windows >=> pure . filter isValidWindow
