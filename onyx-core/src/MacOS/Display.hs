{-# LANGUAGE LambdaCase #-}

-- | MacOS display management types and functions.

module MacOS.Display
  ( DisplayID
  , displays
  , MacOS.Display.activeDisplay
  , realDisplayRect
  , displayRect
  ) where

import Onyx.Prelude

import AppleSdk
       (DisplayID, Rect, activeDisplay, displayCount, activeDisplays,
        menuBarAutohides, dockAutohides, dockSize, displayBounds)
import RectUtils
import MacOS.Internal.Type
import Lens.Micro.Platform (over)
import ExceptExtra (MonadError)

activeDisplay :: MonadIO m => m DisplayID
activeDisplay = liftIO AppleSdk.activeDisplay

displayRect :: MonadIO m => DisplayID -> m Rect
displayRect = liftIO . realDisplayRect

-- | List the 'DisplayID' of all known displays.
displays :: (MonadMac m, MonadError MacError m) => m [DisplayID]
displays = injAction CGError (displayCount >>= activeDisplays)

-- | Rectangle giving the absolute position and size of the display with
-- specified 'DisplayID'.
realDisplayRect :: DisplayID -> IO Rect
realDisplayRect did =
  liftM4 realRect menuBarAutohides dockAutohides
    (fmap ((+) 15) dockSize) (displayBounds did)
  where
    mbh = 22.0 -- menu bar height
    realRect :: Bool -> Bool -> Double -> Rect -> Rect
    realRect mb db dsz =
      if db then id else over sizeL (addToHeight (-dsz)) .
      if mb then id else
        over originL (addToY mbh) . over sizeL (addToHeight (-mbh))
