{-# LANGUAGE RecordWildCards #-}

module RectUtils where

import AppleSdk.Framework
import Lens.Micro.Platform

originL :: Lens' Rect Point
originL = lens origin $ \r p -> case r of Rect _ s -> Rect p s

sizeL :: Lens' Rect Size
sizeL = lens size $ \r s -> case r of Rect o _ -> Rect o s

xL, yL :: Lens' Point Double
xL = lens xCoord $ \p x -> case p of Point _ y -> Point x y
yL = lens yCoord $ \p y -> case p of Point x _ -> Point x y

widthL, heightL :: Lens' Size Double
widthL = lens width $ \s w -> case s of Size _ h -> Size w h
heightL = lens height $ \s h -> case s of Size w _ -> Size w h

addToX, addToY :: Double -> Point -> Point
addToX d = over xL (+d)
addToY d = over yL (+d)

addToWidth, addToHeight :: Double -> Size -> Size
addToWidth d = over widthL (+d)
addToHeight d = over heightL (+d)

halfHeight, halfWidth :: Size -> Size
halfHeight Size {..} = Size width (height / 2)
halfWidth Size {..} = Size (width / 2) height

pad :: Double -> Rect -> Rect
pad p = over originL (over xL (+p) . over yL (+p)) .
        over sizeL (over widthL f . over heightL f) where f x = x - p * 2

splitHorizontallyN, splitVerticallyN :: Int -> Rect -> [Rect]
splitHorizontallyN n r =
  [ Rect (addToY (hPiece * (fromIntegral x)) p) (heightL .~ hPiece $ size r)
  | x <- [0 .. (n - 1)] ]
  where hPiece = r ^.sizeL.heightL / fromIntegral n ; p = origin r
splitVerticallyN n r =
  [ Rect (addToX (wPiece * (fromIntegral x)) p) (widthL .~ wPiece $ size r)
  | x <- [0 .. (n - 1)] ]
  where wPiece = (width . size $ r) / (fromIntegral n) ; p = origin r

splitHorizontally, splitVertically :: Double -> Rect -> (Rect, Rect)
splitHorizontally ratio r =
  (Rect p (over heightL (* ratio) sz) ,
   Rect (addToY halfH $ p) (over heightL (* (1 - ratio)) sz))
  where halfH = (height . size $ r) * ratio ; p = origin r ; sz = size r
splitVertically ratio r =
  (Rect p (over widthL (* ratio) sz) ,
   Rect (addToX halfW $ p) (over widthL (* (1 - ratio)) sz))
  where halfW = (width . size $ r) * ratio ; p = origin r ; sz = size r

data XRelativePos = RightOf | LeftOf deriving Show
data YRelativePos = AboveOf | BelowOf deriving Show

data RelativePos = RPos
  { xAxisRPos :: XRelativePos
  , yAxisRPos :: YRelativePos
  } deriving Show

center :: Rect -> Point
center r =
  Point ((xCoord . origin $ r) + ((width  . size $ r) / 2))
        ((yCoord . origin $ r) + ((height . size $ r) / 2))

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) =
  sqrt (((x2 - x1) ^ (2 :: Int)) + ((y2 - y1) ^ (2 :: Int)))

rectDist :: Rect -> Rect -> Double
rectDist r1 r2 = dist (center r2) (center r1)

rposWrtX :: Point -> Point -> XRelativePos
rposWrtX p q = if (xCoord p - xCoord q) >= 0 then RightOf else LeftOf

rposWrtY :: Point -> Point -> YRelativePos
rposWrtY p q = if (yCoord p - yCoord q) >= 0 then BelowOf else AboveOf

rposWrt :: Point -> Point -> RelativePos
rposWrt p q = RPos (rposWrtX p q) (rposWrtY p q)
