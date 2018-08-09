{-# LANGUAGE LambdaCase #-}

-- | MacOS events.

module MacOS.Internal.Event
  ( Event(..)
  , EventQueue
  )
  where

import AppleSdk (WindowID, DisplayID, PID, KeyPress, processPID)
import qualified MacOS.Internal.LLEvent as LL
       (LLAppEvent(..), LLWindowEvent(..), LLSpaceEvent(..),
        LLDisplayEvent(..), LLKeyboardEvent(..), writeQueue)
-- import Control.Monad.Trans (liftIO)
-- import Control.Monad.Reader
import Control.Concurrent.STM.TQueue
import MacOS.Internal.Window (Window)
import MacOS.Internal.App (App)

type EventQueue = TQueue Event

-- | MacOS event.
data Event
  = WindowCreated Window
  | WindowDestroyed Window
  | FocusedWindowChanged Window
  | WindowMoved Window
  | WindowResized Window
  | WindowMiniaturized Window
  | WindowDeminiaturized Window
  | WindowDiscovered Window
  | WindowTitleChanged Window
  | AppLaunched App
  | AppTerminated PID [Window]
  | DisplayChanged
  | SpaceChanged
  | AppActivated PID
  | AppDeactivated PID
  | AppHidden PID
  | AppVisible PID
  | DisplayAdded DisplayID
  | DisplayRemoved DisplayID
  | DisplayMoved DisplayID
  | DisplayResized DisplayID
  | KeyDown KeyPress
