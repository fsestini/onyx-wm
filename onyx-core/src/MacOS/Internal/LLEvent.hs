module MacOS.Internal.LLEvent
  ( LLDisplayEvent(..)
  , LLWindowEvent(..)
  , LLAppEvent(..)
  , LLKeyboardEvent(..)
  , LLSpaceEvent(..)
  -- , LLEventQueue
  , newQueue
  , writeQueue
  , readNext
  -- , printEvent
  -- , printEvent'
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TQueue

import AppleSdk.Framework hiding (Event, DisplayMoved, String)

-- | MacOS low level event.
data LLWindowEvent
  = WindowCreated PID WindowID WindowElement
  | WindowDestroyed WindowID
  | FocusedWindowChanged WindowID
  | WindowMoved WindowID
  | WindowResized WindowID
  | WindowMiniaturized WindowID
  | WindowDeminiaturized WindowID
  | WindowTitleChanged WindowID

data LLAppEvent
  = AppLaunched PSN
  | AppTerminated PSN
  | AppActivated PID
  | AppDeactivated PID
  | AppHidden PID
  | AppVisible PID

data LLDisplayEvent
  = DisplayChanged
  | DisplayAdded DisplayID
  | DisplayRemoved DisplayID
  | DisplayMoved DisplayID
  | DisplayResized DisplayID

data LLKeyboardEvent = KeyDown KeyPress
data LLSpaceEvent = SpaceChanged

-- type LLEventQueue = TQueue LLEvent

-- Create a new event queue.
newQueue :: IO (TQueue a)
newQueue = atomically newTQueue

writeQueue :: TQueue a -> a -> IO ()
writeQueue q e = atomically (writeTQueue q e)

-- Event retrieval is a blocking, atomic operation that removes the event from
-- the queue.
readNext :: TQueue a -> IO a
readNext q = atomically (readTQueue q)

-- printEvent :: LLEvent -> String
-- printEvent (WindowCreated pid wid _) =
--   unwords ["WindowCreated", show pid, show wid]
-- printEvent (WindowDestroyed wid) = unwords ["WindowDestroyed", show wid]
-- printEvent (FocusedWindowChanged wid) =
--   unwords ["FocusedWindowChanged", show wid]
-- printEvent (WindowMoved wid) = unwords ["WindowMoved", show wid]
-- printEvent (WindowResized wid) = unwords ["WindowResized", show wid]
-- printEvent (WindowMiniaturized wid) = unwords ["WindowMiniaturized", show wid]
-- printEvent (WindowDeminiaturized wid) =
--   unwords ["WindowDeminiaturized", show wid]
-- printEvent (WindowTitleChanged wid) = unwords ["WindowTitleChanged", show wid]
-- printEvent (AppLaunched psn) = unwords ["AppLaunched", show psn]
-- printEvent (AppTerminated psn) = unwords ["AppTerminated", show psn]
-- printEvent DisplayChanged = "DisplayChanged"
-- printEvent SpaceChanged = "SpaceChanged"
-- printEvent (AppActivated pid) = unwords ["AppActivated", show pid]
-- printEvent (AppDeactivated pid) = unwords ["AppDeactivated", show pid]
-- printEvent (AppHidden pid) = unwords ["AppHidden", show pid]
-- printEvent (AppVisible pid) = unwords ["AppVisible", show pid]
-- printEvent (DisplayAdded did) = unwords ["DisplayAdded", show did]
-- printEvent (DisplayRemoved did) = unwords ["DisplayRemoved", show did]
-- printEvent (DisplayMoved did) = unwords ["DisplayMoved", show did]
-- printEvent (DisplayResized did) = unwords ["DisplayResized", show did]
-- printEvent (KeyDown kp) = unwords ["KeyDown", show kp]

-- printEvent' :: LLEvent -> IO ()
-- printEvent' (WindowMoved _) = pure ()
-- printEvent' (WindowResized _) = pure ()
-- printEvent' (WindowTitleChanged _) = pure ()
-- printEvent' e = putStrLn . printEvent $ e
