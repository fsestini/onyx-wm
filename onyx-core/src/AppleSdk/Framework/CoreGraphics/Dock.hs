module AppleSdk.Framework.CoreGraphics.Dock
  ( DockPinning
  , DockOrientation
  , dockAutohides
  , dockOrientation
  , dockSize
  , menuBarAutohides
  ) where

import Foreign.C.Types (CBool(..), CFloat(..), CInt(..))
import Foreign.Ptr
import Foreign
import AppleSdk.Framework.CoreGraphics.Connection

data DockPinning
  = DockPinningIgnore
  | DockPinningStart
  | DockPinningMiddle
  | DockPinningEnd
  deriving (Eq, Show)

type CoreDockPinning = CInt

-- fromDockPinning :: DockPinning -> CoreDockPinning
-- fromDockPinning DockPinningIgnore = 0
-- fromDockPinning DockPinningStart = 1
-- fromDockPinning DockPinningMiddle = 2
-- fromDockPinning DockPinningEnd = 3

data DockOrientation
  = DockIgnore
  | DockTop
  | DockBottom
  | DockLeft
  | DockRight
  deriving (Show)

type CoreDockOrientation = CInt

toDockOrientation :: CoreDockOrientation -> DockOrientation
toDockOrientation cdo
  | cdo == 0 = DockIgnore
  | cdo == 1 = DockTop
  | cdo == 2 = DockBottom
  | cdo == 3 = DockLeft
  | cdo == 4 = DockRight
  | otherwise = error "unknown dock orientation code"

-- fromDockOrientation :: DockOrientation -> CoreDockOrientation
-- fromDockOrientation DockIgnore = 0
-- fromDockOrientation DockTop = 1
-- fromDockOrientation DockBottom = 2
-- fromDockOrientation DockLeft = 3
-- fromDockOrientation DockRight = 4

foreign import ccall unsafe "CoreDockGetAutoHideEnabled"
  coreDockGetAutoHideEnabled :: IO CBool

dockAutohides :: IO Bool
dockAutohides = fmap toBool coreDockGetAutoHideEnabled

foreign import ccall unsafe "CoreDockGetOrientationAndPinning"
  coreDockGetOrientationAndPinning
  :: Ptr CoreDockOrientation -> Ptr CoreDockPinning -> IO ()

dockOrientation :: IO DockOrientation
dockOrientation = alloca $ \p -> alloca $ \q ->
  coreDockGetOrientationAndPinning p q >> fmap toDockOrientation (peek p)

foreign import ccall unsafe "CoreDockGetTileSize"
  coreDockGetTileSize :: IO CFloat

dockSize :: IO Double
dockSize = do
  ratio <- coreDockGetTileSize
  pure . realToFrac $ (ratio * (maxSize - minSize)) + minSize
  where maxSize = 128 ; minSize = 16

foreign import ccall unsafe "CGSGetMenuBarAutohideEnabled"
  cgSGetMenuBarAutohideEnabled :: ConnectionID -> Ptr CInt -> IO ()

menuBarAutohides :: IO Bool
menuBarAutohides = alloca $ \p -> do
  dc <- defaultConnection
  cgSGetMenuBarAutohideEnabled dc p
  fmap (== 1) $ peek p
