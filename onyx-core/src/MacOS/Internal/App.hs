module MacOS.Internal.App where

import Control.Monad (void)
import MacOS.Internal.Process
import qualified MacOS.Internal.LLEvent as LL
import AppleSdk hiding (String)
import Foreign.StablePtr

data App = App
  { appProcess :: CarbonProcess
  , appElement :: AppElement
  , appObserver :: Observer
  , _appPID :: PID
  }

-- | Register a UI notification to the application's internal observer.
registerAppNotification :: App -> UINotification -> AX ()
registerAppNotification App {..} notif =
  (liftIO (newStablePtr pid)) >>= addNotification appObserver appElement notif
  where pid = crbnPID appProcess

-- | Remove application observers from the specified UI notification.
removeAppNotification :: App -> UINotification -> AX ()
removeAppNotification App {..} = removeNotification appObserver appElement

-- | PID of an application's associated process.
appPID :: App -> PID
appPID app = crbnPID (appProcess app)

-- samePID :: App -> App -> Bool
-- samePID app1 app2 = appPID app1 == appPID app2

-- hasPID :: PID -> App -> Bool
-- hasPID pid app = appPID app == pid

-- -- | Name of the application.
-- --
-- -- Since an application's name can change over time, the function retrieves it
-- -- from scratch at each invocation. Hence the side effect.
-- appName :: App -> IO String
-- appName = processName . appProcess

createApp :: (LL.LLWindowEvent -> IO ()) -> CarbonProcess -> AX App
createApp f p = do
  obs <- observerCreate (crbnPID p) (observerHandler f)
  app <- liftIO $ createApplicationUIElement (crbnPID p)
  pure $ App p app obs (crbnPID p)

startObserver :: Observer -> IO ()
startObserver obs = do
  mrl <- mainRunLoop
  rls <- observerGetRunLoopSource obs
  mode <- runLoopDefaultMode
  b <- runLoopContainsSource mrl rls mode
  if b then pure () else runLoopAddSource mrl rls mode

stopObserver :: Observer -> IO ()
stopObserver obs = observerGetRunLoopSource obs >>= runLoopSourceInvalidate

-- | Stop event observer associated to the app.
stopAppObserver :: App -> IO ()
stopAppObserver = stopObserver . appObserver

-- | Start the event observer associated to the app.
startAppObserver :: App -> IO ()
startAppObserver = startObserver . appObserver

observerHandler :: (LL.LLWindowEvent -> IO ()) -> ObserverCallback
observerHandler f _ el notif ptr = case notif of
  
  WindowCreatedNotification -> do
    pid <- deRefStablePtr (castPtrToStablePtr . castStablePtrToPtr $ ptr)
    void . runAction $ do
      wid <- getElementWindow el
      liftIO $ f (LL.WindowCreated pid wid el)

  -- When a window is destroyed, it is impossible to retrieve
  -- its Window ID through the AX API.
  -- Hence we store it as user payload.
  UIElementDestroyedNotification ->
    deRefStablePtr (castPtrToStablePtr . castStablePtrToPtr $ ptr)
      >>= f . LL.WindowDestroyed

  -- We have to make sure that we can actually interact
  -- with the window.
  -- When a window is created, we receive this notification
  -- before kAXWindowCreatedNotification.
  -- When a window is deminimized, we receive this notification
  -- before the window is visible.

  -- To achieve the expected behaviour, we might want to emit
  -- a WindowFocused event after processing WindowCreated
  -- WindowDeminimized events.
  FocusedWindowChangedNotification -> throwAwayAX $
    getElementWindow el >>= liftIO . f . LL.FocusedWindowChanged

  WindowMovedNotification -> throwAwayAX $
    getElementWindow el >>= liftIO . f . LL.WindowMoved

  WindowResizedNotification -> throwAwayAX $
    getElementWindow el >>= liftIO . f . LL.WindowResized

  WindowMiniaturizedNotification -> throwAwayAX $ do
    liftIO (putStrLn "WindowMiniaturizedNotification")
    getElementWindow el >>= liftIO . f . LL.WindowMiniaturized

  -- When a deminimized window pulls the user to the space
  -- of that window, we receive this notification before
  -- ActiveSpaceChangeNotification.
  --
  -- This does NOT happen if a window is deminimized by
  -- cmd-clicking the window. The window will be deminimized
  -- on the currently active space, and no space change occur.
  WindowDeminiaturizedNotification -> throwAwayAX $ do
    liftIO (putStrLn "WindowDeminiaturizedNotification")
    getElementWindow el >>= liftIO . f . LL.WindowDeminiaturized

  TitleChangedNotification -> throwAwayAX $
    getElementWindow el >>= liftIO . f . LL.WindowTitleChanged

  where throwAwayAX = void . runAction
