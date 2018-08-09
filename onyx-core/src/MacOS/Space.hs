-- | MacOS Spaces management types and functions.

module MacOS.Space
  ( Space(..)
  , SpaceType(..)
  , SpaceID
  , activeSpace
  , activeSpaceForDisplay
  , spaceHasWindow
  , spaceForWindow
  , moveToSpace
  , startWorkspaceEvents
  , allSpaces
  , windowsInSpace
  ) where

import Onyx.Prelude

import AppleSdk
       (WindowID, SpaceID, SpaceType(..), startWorkspaceEvents,
        Dictionary, String, Number, displayUUID, uuidString',
        currentSpace', spaceType', displayForWindow', IOIso(..),
        defaultConnection, managedDisplaySpaces, arrayValues, strKey,
        getDictValue, numberGetValue, NumberType(..), numberCreate',
        createArray', spacesForWindows, SpaceSelector(..), moveWindowsToSpace)
import MacOS.Internal.Window (Window(..))
import MacOS.Internal.Type
import MacOS.Display
import MacOS.Window

import Control.Monad.Loops (whileM_)
import Control.Concurrent (threadDelay)
import ExceptExtra (MonadError)

data Space = Space
  { spcID :: SpaceID
  , spcType :: SpaceType
  } deriving (Show)

displayUUIDString :: DisplayID -> IO AppleSdk.String
displayUUIDString = displayUUID >=> uuidString'

activeSpaceIDForDisplay :: DisplayID -> IO SpaceID
activeSpaceIDForDisplay = displayUUIDString >=> currentSpace'

activeSpaceID :: IO SpaceID
activeSpaceID = activeDisplay >>= activeSpaceIDForDisplay

-- | Return the space that is currently active.
activeSpace :: MonadIO m => m Space
activeSpace = liftIO (activeSpaceID >>= createSpace)

-- | Return the 'SpaceID' of the space that is currently active is the dispaly
-- with given 'DisplayID'.
activeSpaceForDisplay :: MonadIO m => DisplayID -> m Space
activeSpaceForDisplay = liftIO . (>>= createSpace) . activeSpaceIDForDisplay

createSpace :: SpaceID -> IO Space
createSpace sid = Space sid <$> spaceType' sid

-- | Returns the 'SpaceID' of the space to which the window with a given
-- 'WindowID' belongs to.
spaceIDForWindowID :: WindowID -> IO SpaceID
spaceIDForWindowID = displayForWindow' >=> currentSpace'

spaceForWindowID :: WindowID -> IO Space
spaceForWindowID = spaceIDForWindowID >=> createSpace

-- | Returns the space to which the specified window belongs to.
spaceForWindow :: MonadIO m => Window -> m Space
spaceForWindow = liftIO . spaceForWindowID . _windowID

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p xs = fmap (headMay . catMaybes) . sequence $
  fmap (\x -> p x >>= \b -> pure (if b then Just x else Nothing)) xs

strComp :: AppleSdk.String -> AppleSdk.String -> IO Bool
strComp str1 str2 = liftM2 (==) (ioFrom str1 :: IO Prelude.String) (ioFrom str2)

allSpaces' :: IO [(AppleSdk.String, [Space])]
allSpaces' = do
  conn <- defaultConnection
  arr <- managedDisplaySpaces conn
  dicts <- arrayValues arr :: IO [Dictionary]
  forM dicts $ \dict -> do
    did <- strKey "Display Identifier" >>= getDictValue dict :: IO AppleSdk.String
    sarr <- strKey "Spaces" >>= getDictValue dict
    spaceDicts <- arrayValues sarr :: IO [Dictionary]
    ss <- forM spaceDicts $ \space -> do
      num <- strKey "id64" >>= getDictValue space :: IO Number
      msid <- fmap fromIntegral <$>
        numberGetValue num NumberSigned64Type :: IO (Maybe SpaceID)
      maybe (pure Nothing) (\sid -> spaceType' sid >>= \sty ->
        pure (Just (Space sid sty))) msid
    pure (did, catMaybes ss)

-- | Returns whether the space currently contains the specified window.
spaceHasWindow :: MonadIO m => Space -> Window -> m Bool
spaceHasWindow sp w = liftIO (spaceIDHasWindowID (spcID sp) (_windowID w))

windowsInSpace :: MonadMac m => Space -> m [Window]
windowsInSpace sp = wins >>= filterM (spaceHasWindow sp)

spaceIDHasWindowID :: SpaceID -> WindowID -> IO Bool
spaceIDHasWindowID sid wid = do
  num <- numberCreate' NumberIntType (fromIntegral wid)
  arr <- createArray' [num]
  def <- defaultConnection
  spaces <- spacesForWindows def SpaceAll arr
  spaceIds <- arrayValues spaces :: IO [Number]
  mays <- fmap catMaybes $
    mapM (fmap (fmap fromIntegral) . flip numberGetValue NumberIntType) spaceIds
  pure $ any (== sid) mays

-- | Move the window with given 'WindowID' to the space with given 'SpaceID'.
spaceMoveWindow :: MonadIO m => SpaceID -> WindowID -> m ()
spaceMoveWindow sid wid = liftIO $ do
  wid' <- numberCreate' NumberIntType (fromIntegral wid)
  arr <- createArray' [wid']
  c <- defaultConnection
  moveWindowsToSpace c arr sid

allSpaces :: (MonadMac m, MonadError MacError m) => m [(DisplayID, [Space])]
allSpaces = do
  dids <- displays
  liftIO $ do
    sps <- allSpaces'
    x <- forM dids $ \did -> do
      uuid <- displayUUIDString did
      m <- findM (strComp uuid . fst) sps
      pure $ fmap ((did,) . snd) m
    pure $ catMaybes x

moveToSpace :: (MonadMac m, MonadError MacError m) => Space -> Window -> m ()
moveToSpace sp w = do
  r <- windowRect w
  liftIO (spaceMoveWindow (spcID sp) (_windowID w))
  whileM_ (fmap (== r) (windowRect w)) (liftIO (threadDelay 100))
