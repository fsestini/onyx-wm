{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Onyx.Layouts.TwoPaned
  ( Tall(..)
  , TwoPanedMsg(..)
  , defaultTall
  , masterPanes
  , orientation
  , splitRatio
  ) where

import Onyx.Prelude

import Utils
import MacOS
import RectUtils
import ExceptExtra
import Data.List.NonEmptyZipper
       (NonEmptyZipper(..), getPosition, _current, previousMod, nextMod)
import qualified Data.Tuple as T (swap)
import Control.Applicative ((<|>))
import Onyx.Core
import Lens.Micro.Platform

data PaneOrder = MS | SM deriving (Eq, Show)

data Orientation = Horizontal PaneOrder | Vertical PaneOrder
  deriving (Eq, Show)

data Tall = Tall
  { _masterPanes :: Int
  , _orientation :: Orientation
  , _splitRatio :: Double
  } deriving Show
makeLenses ''Tall

defaultTall :: Tall
defaultTall = Tall 1 (Vertical MS) 0.5

instance Layout Tall where
  render r = onDoublePane [] (\(TW w _) -> [(w, r)]) (renderTwoPaned r)
  handleMsg m = fromMaybe (const2 (pure Nothing))
    (fmap (pure .:. handleTPMsg) (castMsg m) <|> fmap handleStdMsg (castMsg m))

onDoublePane
  :: a
  -> (TiledWindow -> a)
  -> (Orientation -> Double -> [TiledWindow] -> [TiledWindow] -> a)
  -> Tall -> [TiledWindow] -> a
onDoublePane z _ _ _ [] = z
onDoublePane _ f _ _ [x] = f x
onDoublePane _ _ g (Tall n o s) xs = g o s (take n xs) (drop n xs)

--------------------------------------------------------------------------------
-- Messages and handlers

nezFindW :: Window -> [TiledWindow] -> Maybe (NonEmptyZipper TiledWindow)
nezFindW w = nezFind (sameWID w . _twWindow)

data TwoPanedMsg
  = IncrSplit | DecrSplit | RotateClockwise | EqualizeSplit
  | SwapMaster Window | ExpandMaster | ShrinkMaster
instance Message TwoPanedMsg

onTall :: (a -> a) -> a -> b -> Maybe (a, b)
onTall f = Just . first f .: (,)

handleStdMsg :: StdMessage -> Tall -> [TiledWindow] -> Onyx (Maybe (Tall, [TiledWindow]))
handleStdMsg (InsertNew newW) = \t tws -> let n = _masterPanes t in
  pure (Just (t, take n tws ++ [newW] ++ drop n tws))

handleStdMsg (FocusTowards w dir) =

  fmap (const Nothing) . maybe (pure ()) (logAndContinue () . focusWindow . _twWindow) .:
  onDoublePane Nothing (const Nothing) (\o _ tw1 tw2 -> findTw w o dir tw1 tw2)

handleStdMsg (SwapTowards w dir) = \t tws -> pure $ do
  m <- onDoublePane  Nothing (const Nothing)
         (\o _ tw1 tw2 -> findTw w o dir tw1 tw2) t tws
  m' <- find (sameWID w . _twWindow) tws
  let f x | sameWID (_twWindow x) w = m
          | sameWID (_twWindow x) (_twWindow m) = m'
          | otherwise = x
  pure (t, fmap f tws)

findTw
  :: Window -> Orientation -> Direction -> [TiledWindow] -> [TiledWindow]
  -> Maybe TiledWindow
findTw w o dir tw1 tw2 = case dir of
  DirNext  -> _current . nextMod <$> nezFindW w tws
  DirPrev  -> _current . previousMod <$> nezFindW w tws
  DirLeft  -> leftOf w o tw1 tw2
  DirRight -> rightOf w o tw1 tw2
  DirUp    -> upOf w o tw1 tw2
  DirDown  -> downOf w o tw1 tw2
  where tws = tw1 ++ tw2

handleTPMsg :: TwoPanedMsg -> Tall -> [TiledWindow] -> Maybe (Tall, [TiledWindow])
handleTPMsg IncrSplit = onTall incrSplit
handleTPMsg DecrSplit = onTall decrSplit
handleTPMsg RotateClockwise = onTall (over orientation rotateOrntClockwise)
handleTPMsg EqualizeSplit = onTall resetSplit
handleTPMsg ExpandMaster = \l -> Just . ((expandMaster l . length . toList) &&& id)
handleTPMsg ShrinkMaster = onTall shrinkMaster
handleTPMsg (SwapMaster slaveID) = \t ->
  fmap (t,) . onDoublePane Nothing (const Nothing) (const2 (swapMaster slaveID)) t

expandMaster :: Tall -> Int -> Tall
expandMaster t@(Tall m x y) tot =
  if tot - m > 1 then Tall (succ m) x y else t

shrinkMaster :: Tall -> Tall
shrinkMaster (Tall m x y) = Tall (if m == 1 then 1 else pred m) x y

swapMaster :: Window -> [TiledWindow] -> [TiledWindow] -> Maybe [TiledWindow]
swapMaster slave mst slv = do
  i <- elemIndex (windowID slave) (fmap twWid slv)
  pure $ swapAt 0 (length mst + i) (mst ++ slv)

rect :: Double -> Double -> Double -> Double -> Rect
rect x y w h = Rect (Point x y) (Size w h)

fakePanes :: Orientation -> (Rect, Rect)
fakePanes (Vertical _) = (rect 0 0 100 100, rect 100 0 100 100)
fakePanes (Horizontal _) = (rect 0 0 100 100, rect 0 100 100 100)

incrSplit, decrSplit, resetSplit :: Tall -> Tall
incrSplit = over splitRatio incrCapped
  where incrCapped x = if x + 0.05 > 0.8 then 0.8 else x + 0.05
decrSplit = over splitRatio decrCapped
  where decrCapped x = if x - 0.05 < 0.2 then 0.2 else x - 0.05
resetSplit = over splitRatio (const 0.5)

otherOf' :: NonEmptyZipper TiledWindow -> [TiledWindow] -> Maybe TiledWindow
otherOf' z tws = closest
  where
    ornt = Vertical MS ; (fp1, fp2) = fakePanes ornt
    (ts1, ts2) = (renderPane ornt fp1 (toList z), renderPane ornt fp2 tws)
    r = snd (ts1 !! getPosition z)
    closest = fmap fst . headMay $ sortBy (on compare (rectDist r . snd)) ts2

otherOf :: Window -> [TiledWindow] -> [TiledWindow] -> Maybe TiledWindow
otherOf w tws1 tws2 =  (nezFindW w tws1 >>= \z -> otherOf' z tws2)
                   <|> (nezFindW w tws2 >>= \z -> otherOf' z tws1)

renderTwoPaned
  :: Rect -> Orientation -> Double
  -> [TiledWindow] -> [TiledWindow] -> [(Window, Rect)]
renderTwoPaned screen ornt fl mstr slv = fmap (first _twWindow) $
  renderPane ornt masterRect mstr ++ renderPane ornt slaveRect slv
  where (masterRect, slaveRect) = paneRects ornt fl screen

renderPane :: Orientation -> Rect -> [TiledWindow] -> [(TiledWindow, Rect)]
renderPane (Vertical _) r tws = zip tws (splitHorizontallyN (length tws) r)
renderPane (Horizontal _) r tws = zip tws (splitVerticallyN (length tws) r)

paneRects :: Orientation -> Double -> Rect -> (Rect, Rect)
paneRects (Vertical MS) fl = splitVertically fl
paneRects (Vertical SM) fl = T.swap . splitVertically fl
paneRects (Horizontal MS) fl = splitHorizontally fl
paneRects (Horizontal SM) fl = T.swap . splitHorizontally fl

leftOf, rightOf, upOf, downOf
  :: Window -> Orientation -> [TiledWindow] -> [TiledWindow] -> Maybe TiledWindow
leftOf w ornt = case ornt of
  { Vertical _ -> otherOf w ; Horizontal _ -> aboveOf w }
rightOf w ornt = case ornt of
  { Vertical _ -> otherOf w ; Horizontal _ -> belowOf w }
upOf w ornt = case ornt of
  { Vertical _ -> aboveOf w ; Horizontal _ -> otherOf w }
downOf w ornt = case ornt of
  { Vertical _ -> belowOf w ; Horizontal _ -> otherOf w }

aboveOf, belowOf :: Window -> [TiledWindow] -> [TiledWindow] -> Maybe TiledWindow
aboveOf w t1 t2 =  ((_current . previousMod) <$> nezFindW w t1)
               <|> ((_current . previousMod) <$> nezFindW w t2)
belowOf w t1 t2 =  ((_current . nextMod) <$> nezFindW w t1)
               <|> ((_current . nextMod) <$> nezFindW w t2)

rotateOrntClockwise :: Orientation -> Orientation
rotateOrntClockwise = \case
  Horizontal MS -> Vertical MS
  Vertical MS   -> Horizontal SM
  Horizontal SM -> Vertical SM
  Vertical SM   -> Horizontal MS
