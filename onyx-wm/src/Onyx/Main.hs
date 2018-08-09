module Onyx.Main where

import Onyx.Prelude hiding (Alt)

import Control.Concurrent (forkIO)
import ExceptExtra
import MacOS
import Data.Set (fromList)
import Onyx.Layouts
import Onyx.Core
import Onyx.Operations
import Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------
-- Default config

data KeyBinding = KB Key [ModifierPress]

toKeyPress :: KeyBinding -> KeyPress
toKeyPress (KB k m) = KPress (toKeycode k) (fromList m)

altKB :: Char -> KeyBinding
altKB c = KB (KeyLiteral c) [(Alt, LeftPos)]

altShiftKB :: Char -> KeyBinding
altShiftKB c = KB (KeyLiteral c) [(Alt, LeftPos), (Shift, LeftPos)]

infix 4 ~~>
(~~>) :: KeyBinding -> m () -> (KeyBinding, m ())
(~~>) = (,)

keyBindings :: [(KeyBinding, EOnyx ())]
keyBindings =
  [ altKB 'j'      ~~> sendFocusMsg DirDown
  , altKB 'k'      ~~> sendFocusMsg DirUp
  , altKB 'h'      ~~> sendFocusMsg DirLeft
  , altKB 'l'      ~~> sendFocusMsg DirRight
  
  , altShiftKB 'j' ~~> sendSwapMsg DirDown
  , altShiftKB 'k' ~~> sendSwapMsg DirUp
  , altShiftKB 'h' ~~> sendSwapMsg DirLeft
  , altShiftKB 'l' ~~> sendSwapMsg DirRight
  
  , altKB 'w'      ~~> (focusedWindow >>= closeWindow)
  , altKB 'r'      ~~> msgToActive RotateClockwise
  , altKB 'n'      ~~> msgToActive DecrSplit
  , altKB 'm'      ~~> msgToActive IncrSplit
  , altShiftKB '=' ~~> msgToActive EqualizeSplit
  , altShiftKB '1' ~~> sendToDisplay 0
  , altShiftKB '2' ~~> sendToDisplay 1
  , altKB '1'      ~~> focusDisplay 0
  , altKB '2'      ~~> focusDisplay 1
  , altKB 'r'      ~~> msgToActive RotateClockwise
  , altKB ','      ~~> msgToActive ExpandMaster
  , altKB '.'      ~~> msgToActive ShrinkMaster
  , altKB 't'      ~~> (focusedWindow >>= tile)
  , altKB 'u'      ~~> (focusedWindow >>= untile)
  , KB KeySpace [(Alt, LeftPos)] ~~> liftOnyx nextLayout
  , KB KeyReturn [(Alt, LeftPos)] ~~> (focusedWindow >>= msgToActive . SwapMaster)
  ]
  where sendFocusMsg d =
          focusedWindow >>= \w -> msgToActive (FocusTowards w d)
        sendSwapMsg d =
          focusedWindow >>= \w -> msgToActive (SwapTowards w d)

handleEvent :: Event -> Onyx ()
handleEvent = (>> renderTree) . \case
  AppLaunched a -> appWindows a >>= mapM_ (handleEvent . WindowCreated)
  AppTerminated _ ws -> mapM_ (handleEvent . WindowDestroyed) ws

  WindowCreated w -> justLog $ do
    b <- isWindowTileable w
    if b then dispatchNew w else pure ()
  WindowDiscovered w ->  justLog $ do
    b <- isWindowTileable w
    if b then dispatchNew w else pure ()
  WindowDestroyed w -> justLog (untile w)
  
  KeyDown kp -> maybe (liftIO (putStrLn "key not bound")) justLog $
    lookup kp (fmap (first toKeyPress) keyBindings)

  _ -> pure ()
  
--------------------------------------------------------------------------------

onyxMain :: IO ()
onyxMain = do
  macInitIO
  _ <- forkIO . runMac . runOnyx c . logAndContinue () $ do
    installHandlers (fmap (toKeyPress . fst) keyBindings)
    populateInitialAppCache
    lift $ renderTree >> forever (nextEvent >>= handleEvent)
  runLoop
  where
    c = OnyxConfig (OptIn ["Terminal", "Emacs"]) 10
                     (SL defaultTall :| [SL Maximized])
