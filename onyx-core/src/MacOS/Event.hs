{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MacOS.Event
  ( macInit
  , macInitIO
  , populateInitialAppCache
  , nextEvent
  , installHandlers
  , Event(..)
  ) where

import Onyx.Prelude

import Control.Concurrent
import ExceptExtra (logAndContinue, MonadError(..), ExceptT(..), runExceptT)
import AppleSdk
       (AX, UINotification(..), runAction, startWorkspaceEvents, KeyPress,
        PID, processPID)
import qualified MacOS.Internal.LLEvent as LL
import MacOS.Internal.Event
import MacOS.Window
import MacOS.Internal.Window
import MacOS.Internal.Process
import MacOS.Internal.App
import MacOS.Internal.Workspace
import MacOS.Internal.Display
import MacOS.Internal.Keyboard
import MacOS.Internal.Type

nextEvent :: MonadMac m => m Event
nextEvent = viewMac queue >>= liftIO . LL.readNext

installHandler
  :: (MonadMac m, MonadError MacError m) => IO Bool -> HandlerError -> m ()
installHandler h e = do
  b <- liftIO h
  if b then pure () else throwError (HandlerError e)

installHandlers :: (MonadMac m, MonadError MacError m) => [KeyPress] -> m ()
installHandlers kps = do
  r <- askMac
  
  installHandler (installCarbonHandler (`processAppEvent` r)) CarbonHandlerError

  installHandler
    ((>> pure True) $ installWorkspaceHandler
     (WH (`processDisplayEvent` r) (`processSpaceEvent` r) (`processAppEvent` r)))
        WorkspaceHandlerError

  installHandler
    (fmap (either (const False) (const True)) . runAction $
      installDisplayHandler (`processDisplayEvent` r)) DisplayHandlerError
  
  liftIO startWorkspaceEvents
  listenTo kps

listenTo :: (MonadMac m, MonadError MacError m) => [KeyPress] -> m ()
listenTo kps = askMac >>= \r ->
  installHandler (installKeyboardHandler (flip processKeyboardEvent r) kps) KeyHandlerError

loop :: Monad m => (a -> Bool) -> a -> [m a] -> m a
loop _ x [] = pure x
loop p x (m : ms) = do
  y <- m
  if p y then pure y else loop p x ms

tryDelayedAtMost :: MonadIO m => Int -> Int -> AX () -> ExceptT MacError m ()
tryDelayedAtMost times delay m = ExceptT . liftIO $
  loop (either (const False) (const True)) (Left TimeoutError) $
  intersperse
    (threadDelay delay >> pure (Left TimeoutError))
    (replicate times (bimap AXError id <$> runAction m))

writeMacEvent :: MonadMac m => Event -> m ()
writeMacEvent e = viewMac queue >>= liftIO . flip LL.writeQueue e

appNotifs :: [UINotification]
appNotifs =
  [ WindowCreatedNotification
  , FocusedWindowChangedNotification
  ]

windowNotifs :: [UINotification]
windowNotifs =
  [ UIElementDestroyedNotification
  , WindowMiniaturizedNotification
  , WindowDeminiaturizedNotification
  ]

processAppEvent :: LL.LLAppEvent -> MacConfig -> IO ()
processAppEvent e =
  fmap ((>> pure ()) . forkIO) . runReaderT . unMac .
  logAndContinue () . join . fmap writeMacEvent $ case e of
  
  LL.AppLaunched psn -> do
    r <- askMac
    p <- liftIO (carbonProcess psn)

    app <- injAction AXError $ createApp (flip processWindowEvent r) p
    overApps (app :)

    tryDelayedAtMost 10 1000000 (registerAppNotifs app)
    liftIO (startAppObserver app)
    _ <- lift (updateWinCacheForApp app)

    pure (AppLaunched app)

  LL.AppTerminated psn -> do
    pid <- liftIO (processPID psn)
    ws <- fmap (filter ((== pid) . _appPID . _windowParent)) wins
    lift (liftIO (processPID psn) >>= releaseApp)
    
    pure $ AppTerminated pid ws

  LL.AppActivated pid -> pure $ AppActivated pid
  LL.AppDeactivated pid -> pure $ AppDeactivated pid
  LL.AppHidden pid -> pure $ AppHidden pid
  LL.AppVisible pid -> pure $ AppVisible pid

  where
    registerAppNotifs a = forM_ appNotifs (registerAppNotification a)

processWindowEvent :: LL.LLWindowEvent -> MacConfig -> IO ()
processWindowEvent e =
  fmap (>> pure ()) . runReaderT . unMac . runExceptT . join .
  fmap (maybe (pure ()) writeMacEvent) $ case e of
  
  LL.WindowCreated pid _ wref -> do
    app <- findByPID pid
    win <- injAction AXError $ createWindow app wref
    if isValidWindow win
      then do
        injAction AXError $ registerWindowNotifs app win
        overWins (win :)
        pure (Just (WindowCreated win))

      else pure Nothing

  LL.WindowDestroyed wid -> do
    w <- findByWID wid
    logAndContinue () (releaseWindow w)
    
    pure (Just (WindowDestroyed w))

  LL.FocusedWindowChanged w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . FocusedWindowChanged)
  LL.WindowMoved w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . WindowMoved)
  LL.WindowResized w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . WindowResized)
  LL.WindowMiniaturized w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . WindowMiniaturized)
  LL.WindowDeminiaturized w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . WindowDeminiaturized)
  LL.WindowTitleChanged w ->
    logAndContinue Nothing (findByWID w >>= pure . Just . WindowTitleChanged)

  where
    registerWindowNotifs app' w =
      registerWindowNotifications app' w windowNotifs

processDisplayEvent :: LL.LLDisplayEvent -> MacConfig -> IO ()
processDisplayEvent = runReaderT . unMac . writeMacEvent . \case
  LL.DisplayChanged -> DisplayChanged
  LL.DisplayAdded d -> DisplayAdded d
  LL.DisplayRemoved d -> DisplayRemoved d
  LL.DisplayMoved d -> DisplayMoved d
  LL.DisplayResized d -> DisplayResized d

processKeyboardEvent :: LL.LLKeyboardEvent -> MacConfig -> IO ()
processKeyboardEvent (LL.KeyDown kp) = runReaderT . unMac $ writeMacEvent (KeyDown kp)

processSpaceEvent :: LL.LLSpaceEvent -> MacConfig -> IO ()
processSpaceEvent LL.SpaceChanged =
  runReaderT . unMac $ do
    ws <- apps >>= mapM updateWinCacheForApp
    writeMacEvent SpaceChanged
    forM_ (concat ws) (writeMacEvent . WindowDiscovered)

-- stop handlers and remove app from list
releaseApp :: MonadMac m => PID -> m ()
releaseApp pid = do
  logAndContinue () $ do
    app <- findByPID pid
    injAction AXError (removeAppNotifs app)
    liftIO (stopAppObserver app)
  overApps (filter ((/= pid) . appPID))
  where removeAppNotifs = flip removeAppNotification WindowCreatedNotification

-- stop handlers and remove window from list
releaseWindow :: (MonadMac m, MonadError MacError m) => Window -> m ()
releaseWindow w = do
  let app = _windowParent w
  injAction AXError (removeWindowNotifs app w)
  overWins (filter ((/= (_windowID w)) . _windowID))
  where removeWindowNotifs a w' =
          removeWindowNotifications a w' [UIElementDestroyedNotification]

updateAppCache :: MonadMac m => m [App]
updateAppCache = do
  r <- askMac
  
  procs <- liftIO $ do
    ps <- interactiveProcs
    catMaybes <$> mapM (fmap (either (const Nothing) Just) .
                        runAction . createApp (flip processWindowEvent r)) ps

  toAdd <- filterM (fmap not . isAppCached . _appPID) procs
  forM_ toAdd $ \app -> logAndContinue () $ do
    injAction AXError (registerAppNotifs app)
    liftIO (startAppObserver app)

  forM_ toAdd (overApps . (:)) >> pure toAdd
  
  where
    registerAppNotifs a = forM_ appNotifs (registerAppNotification a)

updateWinCacheForApp :: MonadMac m => App -> m [Window]
updateWinCacheForApp app = logAndContinue [] $ do

  winz <- injAction AXError (getValidWindows app)
  toAdd <- lift $ do
    toAdd <- filterM (fmap not . isWinCached . _windowID) winz
    forM_ toAdd $ \w -> logAndContinue () (injAction AXError (registerWindowNotifs app w))
    forM_ toAdd (overWins . (:))
    
    pure toAdd

  forM_ toAdd $ \win -> windowTitle win >>= liftIO . putStrLn . ("Adding win to cache: " ++)
  pure toAdd
    
  where
    registerWindowNotifs app' w =
      registerWindowNotifications app' w windowNotifs

populateInitialAppCache :: MonadMac m => m ()
populateInitialAppCache = do
  ws <- updateAppCache >>= mapM updateWinCacheForApp
  forM_ (concat ws) (writeMacEvent . WindowDiscovered)

macInit :: MonadMac m => m ()
macInit = liftIO macInitIO >> populateInitialAppCache

-- {-

-- - install handlers
-- - register app notifs
-- - populate initial app cache
--   - for each app
--     - for each window for app
--       - registrate window notifs
--       - populate initial window cache

-- -}

