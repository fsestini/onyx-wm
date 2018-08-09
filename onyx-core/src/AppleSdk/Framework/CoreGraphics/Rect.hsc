{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

#include <Carbon/Carbon.h>

{- | Storable rectangles. -}

module AppleSdk.Framework.CoreGraphics.Rect where

import Foreign
import Foreign.C.Types (CBool(..))
import AppleSdk.Framework.CoreFoundation
import AppleSdk.Framework.Accessibility.Value

data Point = Point { xCoord :: Double, yCoord :: Double } deriving (Eq, Show)
data Size = Size { width :: Double, height :: Double } deriving (Eq, Show)

-- | Type of rectangular shapes.
data Rect = Rect
  { origin :: Point
  -- ^ Origin of the rectangle, corresponding to the top-left corner.
  , size :: Size
  -- ^ Size of the rectangle.
  } deriving (Eq, Show)

instance Storable Point where
  sizeOf _ = #{size CGPoint}
  alignment _ = #{alignment CGPoint}
  peek ptr = do
    x' <- #{peek CGPoint, x} ptr
    y' <- #{peek CGPoint, y} ptr
    return $ Point x' y'
  poke ptr (Point x' y') = do
    #{poke CGPoint, x} ptr x'
    #{poke CGPoint, y} ptr y'

instance Storable Size where
  sizeOf _ = #{size CGSize}
  alignment _ = #{alignment CGSize}
  peek ptr = do
    width' <- #{peek CGSize, width} ptr
    height' <- #{peek CGSize, height} ptr
    return $ Size width' height'
  poke ptr (Size width' height') = do
    #{poke CGSize, width} ptr width'
    #{poke CGSize, height} ptr height'

instance Storable Rect where
  sizeOf _ = #{size CGRect}
  alignment _ = #{alignment CGRect}
  peek ptr = do
    origin' <- #{peek CGRect, origin} ptr
    size' <- #{peek CGRect, size} ptr
    return $ Rect origin' size'
  poke ptr (Rect origin' size') = do
    #{poke CGRect, origin} ptr origin'
    #{poke CGRect, size} ptr size'

foreign import ccall unsafe create_cfpoint :: Ptr Point -> IO CFTypeRef
foreign import ccall unsafe create_cfsize :: Ptr Size -> IO CFTypeRef

foreign import ccall unsafe
  ax_value_get_cgpoint :: AXValueRef -> Ptr Point -> IO CBool
foreign import ccall unsafe
  ax_value_get_cgsize  :: AXValueRef -> Ptr Size -> IO CBool

axValueGetPoint :: AXValueRef -> IO (Maybe Point)
axValueGetPoint ref = alloca $ \p -> do
  res <- ax_value_get_cgpoint ref p
  if Foreign.toBool res then Just <$> peek p else pure Nothing

valueGetPoint :: Value -> IO (Maybe Point)
valueGetPoint = flip withCFPtr axValueGetPoint

axValueGetSize :: AXValueRef -> IO (Maybe Size)
axValueGetSize ref = alloca $ \p -> do
  res <- ax_value_get_cgsize ref p
  if Foreign.toBool res then Just <$> peek p else pure Nothing

valueGetSize :: Value -> IO (Maybe Size)
valueGetSize = flip withCFPtr axValueGetSize

-- | Turn a point value into a Core Foundation object.
createCFPoint :: Point -> IO Object
createCFPoint pt = alloca $ \p ->
  poke p pt >> create_cfpoint p >>= manageCFObj

-- | Turn a size value into a Core Foundation object.
createCFSize :: Size -> IO Object
createCFSize sz = alloca $ \p ->
  poke p sz >> create_cfsize p >>= manageCFObj
