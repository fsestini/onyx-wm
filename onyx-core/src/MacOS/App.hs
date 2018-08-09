-- | MacOS application management types and functions.

module MacOS.App
  ( PID
  -- , UINotification(..)
  -- , registerAppNotification
  -- , removeAppNotification
  -- , appPID
  , appName
  , activeApps
  , isAppHidden
  , focusedApp
  -- , startAppObserver
  -- , stopAppObserver
  -- , createApp
  ) where

import Onyx.Prelude

import AppleSdk.Framework (PID, focusedProcess, processPID)
import MacOS.Internal.App
import MacOS.Internal.Process
import MacOS.Internal.Type

import ExceptExtra (MonadError(..))

-- | Name of the application.
--
-- Since an application's name can change over time, the function retrieves it
-- from scratch at each invocation. Hence the side effect.
appName :: MonadIO m => App -> m String
appName = liftIO . processName . appProcess

activeApps :: MonadMac m => m [App]
activeApps = apps

isAppHidden :: (MonadMac m, MonadError MacError m) => App -> m Bool
isAppHidden a = do
  m <- liftIO . isProcessHidden . appProcess $ a
  maybe (throwError UnspecifiedError) pure m

focusedApp :: (MonadMac m, MonadError MacError m) => m App
focusedApp = liftIO (focusedProcess >>= processPID) >>= findByPID
