-- | 

module MacOS.Internal.Workspace where

import qualified MacOS.Internal.LLEvent as LL
import AppleSdk (WorkspaceCallback, SpaceEvent(..), setWorkspaceCallback)

data WorkspaceHandlers = WH
  { _displayHandler :: LL.LLDisplayEvent -> IO ()
  , _spaceHandler :: LL.LLSpaceEvent -> IO ()
  , _appHandler :: LL.LLAppEvent -> IO ()
  }

workspaceHandler :: WorkspaceHandlers -> WorkspaceCallback
workspaceHandler h SEDisplayChanged _ = _displayHandler h LL.DisplayChanged
workspaceHandler h SESpaceChanged _ = _spaceHandler h LL.SpaceChanged
workspaceHandler h SEAppActivated pid = _appHandler h (LL.AppActivated pid)
workspaceHandler h SEAppDeactivated pid = _appHandler h (LL.AppDeactivated pid)
workspaceHandler h SEAppHidden pid = _appHandler h (LL.AppHidden pid)
workspaceHandler h SEAppVisible pid = _appHandler h (LL.AppVisible pid)

installWorkspaceHandler :: WorkspaceHandlers -> IO ()
installWorkspaceHandler =
  setWorkspaceCallback . workspaceHandler
