{-# LANGUAGE LambdaCase #-}

module MacOS.Internal.Display
  ( installDisplayHandler
  ) where

import qualified MacOS.Internal.LLEvent as LL
import AppleSdk.Framework as Q

displayHandler :: (LL.LLDisplayEvent -> IO ()) -> DisplayReconfigurationCallback
displayHandler f did = \case
  DisplayAdd -> f $ LL.DisplayAdded idid
  DisplayRemove -> f $ LL.DisplayRemoved idid
  DisplayMoved -> f $ LL.DisplayMoved idid
  DisplayDesktopShapeChanged -> f $ LL.DisplayResized idid
  _ -> pure ()
  where idid = fromIntegral did

installDisplayHandler :: (LL.LLDisplayEvent -> IO ()) -> CG ()
installDisplayHandler = cgIO . fmap cgEith . setDisplayCallback . displayHandler
