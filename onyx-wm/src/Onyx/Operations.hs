module Onyx.Operations where

import Onyx.Prelude hiding (at)
import Onyx.Core

import ExceptExtra (MonadError(..))
import MacOS
import RectUtils
import Data.List.NonEmptyZipper (nextMod)
import Lens.Micro.Platform ((%=), view, _Just, at,use)

msgToActive :: (Message msg) => msg -> EOnyx ()
msgToActive m = activeSpace >>= flip sendMsgTo m

sendMsgTo :: (Message msg) => Space -> msg -> EOnyx ()
sendMsgTo sp m = overWorkspace sp (liftOnyx .: handleMsg (SomeMessage m))

renderTree :: Onyx ()
renderTree = justLog $ do
  did <- activeDisplay
  drect <- displayRect did
  sp <- activeSpaceForDisplay did
  pd <- view wmcPadding

  liftOnyx $ checkS (spcID sp)

  withWorkspace sp (liftOnyx . draw . fmap (second (pad pd)) .: render drect)

  where draw = mapM_ (justLog . uncurry setWindowRect)

-- | Switch to next layout for the currently focused space.
nextLayout :: Onyx ()
nextLayout = do
  sid <- fmap spcID activeSpace
  wmsSpaceLayouts . at sid . _Just %= nextMod

--------------------------------------------------------------------------------
-- Tiling operations

dispatchNew :: Window -> EOnyx ()
dispatchNew w = do
  b <- configAllowsTile w
  if b
    then (TW w <$> windowRect w) >>= msgToActive . InsertNew
    else pure ()

configAllowsTile :: Window -> EOnyx Bool
configAllowsTile w = do
  tm <- view wmcTileMode
  appCanTile tm <$> appName (windowParent w)
  where
    appCanTile (OptIn appz) app = app `elem` appz
    appCanTile (OptOut appz) app = app `notElem` appz

tile :: Window -> EOnyx ()
tile w = do
  tw <- TW w <$> windowRect w
  sp <- spaceForWindow w
  sendMsgTo sp (InsertNew tw)

untile :: Window -> EOnyx ()
untile w = do
  m <- fmap (find (sameWID w . _twWindow)) (use wmsTiled)
  flip (maybe (throwError (WinNotManaged (windowID w)))) m $ \tw -> do
    setWindowRect (_twWindow tw) (_twFloatRect tw)
    wmsTiled %= filter (not . sameWID w . _twWindow)

--------------------------------------------------------------------------------
-- Focusing operations

focusedWindow :: EOnyx Window
focusedWindow = focusedApp >>= focusedWindowInApp

focusSpace :: Space -> EOnyx ()
focusSpace oss = withWorkspace oss $ \_ tws ->
  maybe (throwError NoWindowsInSpace) (focusWindow) (fmap _twWindow (headMay tws))

spaceAtDisplay :: Int -> EOnyx Space
spaceAtDisplay i = do
  ds <- displays
  d <- maybe (throwError (NoDisplayIdx i)) pure (atMay ds i)
  activeSpaceForDisplay d

focusDisplay :: Int -> EOnyx ()
focusDisplay = spaceAtDisplay >=> focusSpace

--------------------------------------------------------------------------------
-- Moving operations

sendWindowToDisplay :: Int -> Window -> EOnyx ()
sendWindowToDisplay i w = do
  spaceAtDisplay i >>= flip moveToSpace w
  activeSpace >>= focusSpace

sendToDisplay :: Int -> EOnyx ()
sendToDisplay = (focusedWindow >>=) . sendWindowToDisplay
