module MacOS.Internal.Keyboard where

import AppleSdk.Framework
import qualified MacOS.Internal.LLEvent as LL

import Foreign.Ptr
import Control.Monad
import Data.Bits
import qualified Data.Set as S
import Data.Bool

eventTapCallback :: (LL.LLKeyboardEvent -> IO ()) -> [KeyPress] -> CGEventTapCallBack
eventTapCallback q kp _ ety e ref = case toEventType ety of
  EventTapDisabledByTimeout -> pure e
  EventTapDisabledByUserInput -> do
    p <- fromCFPtr' (castPtr ref)
    enableEventTap p True >> pure e
  EventKeyDown -> do
    
    flags <- cgEventGetFlags e
    key <- cgEventGetIntegerValueField e kCGKeyboardEventKeycode
    b <- keyboardHandler q kp (fromIntegral flags) (fromIntegral key)
    bool (pure e) (pure nullPtr) b

keyboardHandler :: (LL.LLKeyboardEvent -> IO ()) -> [KeyPress] -> KeyCallback
keyboardHandler f wanted kflags kcode =
  if kp `elem` wanted
     then f (LL.KeyDown kp) >> pure True
     else pure False
  where kp = KPress kcode (S.fromList $ toModPress kflags)

installKeyboardHandler :: (LL.LLKeyboardEvent -> IO ()) -> [KeyPress] -> IO Bool
installKeyboardHandler f kp = do
  handle <- eventTapCreate kCGSessionEventTap kCGHeadInsertEventTap
              kCGEventTapOptionDefault (1 `shiftL` (fromIntegral kCGEventKeyDown))
              (eventTapCallback f kp) nullPtr

  withCFPtr handle $ \handle' -> do
    if handle' == nullPtr then pure False else do
      en <- eventTapIsEnabled handle
      if (not en) then pure False else do
        alloc <- nullAllocator
        join $ runLoopAddSource <$> mainRunLoop
                                <*> machPortRunLoopSource alloc handle 0
                                <*> runLoopCommonModes
        pure True
