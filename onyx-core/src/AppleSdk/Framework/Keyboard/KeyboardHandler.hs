module AppleSdk.Framework.Keyboard.KeyboardHandler where

-- import AppleSdk.Framework.Keyboard.Foreign
-- -- import AppleSdk.Framework.Event
-- import AppleSdk.Framework.Types
-- import AppleSdk.Framework.Keyboard.CGEvent
-- import Foreign.Ptr

-- import Data.Word
-- import Control.Monad.State
-- import Data.Bits
-- import qualified Data.Set as S
-- import Data.Bool

-- eventTapCallback :: EventQueue -> [KeyPress] -> CGEventTapCallBack
-- eventTapCallback q kp _ ety e ref = case toEventType ety of
--   EventTapDisabledByTimeout -> pure e
--   EventTapDisabledByUserInput -> eventTapEnable (castPtr ref) True >> pure e
--   EventKeyDown -> do
--     flags <- cgEventGetFlags e
--     key <- cgEventGetIntegerValueField e kCGKeyboardEventKeycode
--     b <- keyboardHandler q kp (fromIntegral flags) (fromIntegral key)
--     bool (pure e) (pure nullPtr) b

-- keyboardHandler :: EventQueue -> [KeyPress] -> KeyCallback
-- keyboardHandler q wanted kflags kcode =
--   if elem kp wanted
--      then writeEvent q (KeyDown kp) >> pure True
--      else pure False
--   where kp = KPress kcode (S.fromList $ toModPress kflags)

-- installKeyboardHandler :: EventQueue -> [KeyPress] -> IO Bool
-- installKeyboardHandler q kp = do
--   callb <- wrapCGEventTapCallBack $ eventTapCallback q kp
--   handle <- cgEventTapCreate kCGSessionEventTap kCGHeadInsertEventTap
--               kCGEventTapOptionDefault (1 `shiftL` (fromIntegral kCGEventKeyDown))
--               callb nullPtr
--   if handle == nullPtr then pure False else do
--     en <- eventTapIsEnabled handle
--     if (not en) then pure False else do
--       alloc <- cfAllocatorDefault
--       join $ cfRunLoopAddSource <$> cfRunLoopGetMain
--                                 <*> cfMachPortCreateRunLoopSource alloc handle 0
--                                 <*> cfRunLoopCommonModes
--       pure True
