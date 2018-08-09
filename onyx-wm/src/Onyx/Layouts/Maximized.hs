module Onyx.Layouts.Maximized where

import MacOS
import Onyx.Prelude
import Onyx.Core
import Utils
import Data.List.NonEmptyZipper (nextMod, previousMod, _current)

data Maximized = Maximized
  deriving (Show)

instance Layout Maximized where
  render r = const (fmap ((,r) . _twWindow))
  handleMsg m t =
    maybe (const (pure Nothing)) (fmap (fmap (t,)) .: handleStdMsgs) (castMsg m)

handleStdMsgs :: StdMessage -> [TiledWindow] -> Onyx (Maybe [TiledWindow])
handleStdMsgs (InsertNew tw) tws = pure (Just (tw : tws))
handleStdMsgs (FocusTowards w dir) tws =
  (>> pure Nothing) . maybe (pure ()) (justLog . focusWindow) $ case dir of
    DirUp -> onCurrent previousMod w tws
    DirLeft -> onCurrent previousMod w tws
    DirDown -> onCurrent nextMod w tws
    DirRight -> onCurrent nextMod w tws
    DirNext -> onCurrent nextMod w tws
    DirPrev -> onCurrent previousMod w tws
  where
    onCurrent f w' tws' =
      fmap (_twWindow . _current . f) (nezFind (sameWID w' . _twWindow) tws')

handleStdMsgs _ _ = pure Nothing
