module AppleSdk.Framework.CoreGraphics.Event where

import Foreign.C.Types (CInt(..), CUInt(..), CULong(..), CBool(..))
import Foreign.Ptr
import Foreign
import System.IO.Unsafe
import AppleSdk.Framework.CoreFoundation

type CGEventType = CInt
type EventFlags = Word64
type CGEventField = CInt

data CGEvent
type CGEventRef = Ptr CGEvent
newtype Event = Event { getEvent :: ForeignPtr CGEvent }

foreign import ccall unsafe "CGSIsSecureEventInputSet"
  check_secure_input_set :: IO CBool

checkSecureInputSet :: IO Bool
checkSecureInputSet = fmap Foreign.toBool check_secure_input_set

foreign import ccall unsafe "CGEventGetFlags"
  cgEventGetFlags :: CGEventRef -> IO EventFlags

eventFlags :: Event -> IO EventFlags
eventFlags = flip withForeignPtr cgEventGetFlags . getEvent

foreign import ccall unsafe "CGEventGetIntegerValueField"
  cgEventGetIntegerValueField :: CGEventRef -> CGEventField -> IO Int64

foreign import ccall unsafe kCGKeyboardEventKeycode_ :: IO CGEventField
kCGKeyboardEventKeycode :: CGEventField
kCGKeyboardEventKeycode = unsafePerformIO kCGKeyboardEventKeycode_
{-# NOINLINE kCGKeyboardEventKeycode #-}

data EventField = KeyboardEventKeycode

fromEventField :: EventField -> CGEventField
fromEventField KeyboardEventKeycode = kCGKeyboardEventKeycode

intValueField :: Event -> EventField -> IO Int64
intValueField (Event e) field =
  withForeignPtr e (flip cgEventGetIntegerValueField (fromEventField field))

--------------------------------------------------------------------------------

type CGEventTapLocation = CInt
type CGEventTapPlacement = CInt
type CGEventTapOptions = CInt

foreign import ccall unsafe mykCGSessionEventTap :: IO CGEventTapLocation
kCGSessionEventTap :: CGEventTapLocation
kCGSessionEventTap = unsafePerformIO mykCGSessionEventTap
{-# NOINLINE kCGSessionEventTap #-}

foreign import ccall unsafe mykCGHeadInsertEventTap :: IO CGEventTapPlacement
kCGHeadInsertEventTap :: CGEventTapPlacement
kCGHeadInsertEventTap = unsafePerformIO mykCGHeadInsertEventTap
{-# NOINLINE kCGHeadInsertEventTap #-}

foreign import ccall unsafe mykCGEventTapOptionDefault :: IO CGEventTapOptions
kCGEventTapOptionDefault :: CGEventTapOptions
kCGEventTapOptionDefault = unsafePerformIO mykCGEventTapOptionDefault
{-# NOINLINE kCGEventTapOptionDefault #-}

data CGEventTapProxy_
type CGEventTapProxy = Ptr CGEventTapProxy_

type CGEventMask = CULong

type CGEventTapCallBack =
  CGEventTapProxy -> CGEventType -> CGEventRef -> Ptr () -> IO CGEventRef

foreign import ccall "wrapper"
  wrapCGEventTapCallBack :: CGEventTapCallBack -> IO (FunPtr CGEventTapCallBack)

foreign import ccall "CGEventTapCreate" cgEventTapCreate
  :: CGEventTapLocation -> CGEventTapPlacement -> CGEventTapOptions
  -> CGEventMask -> FunPtr CGEventTapCallBack -> Ptr () -> IO CFMachPortRef

eventTapCreate
  :: CGEventTapLocation -> CGEventTapPlacement -> CGEventTapOptions
  -> CGEventMask -> CGEventTapCallBack -> Ptr () -> IO MachPort
eventTapCreate l p o m c q = do
  c' <- wrapCGEventTapCallBack c
  cgEventTapCreate l p o m c' q >>= manageCFObj

--------------------------------------------------------------------------------
-- Event types

data EventType
  = EventTapDisabledByTimeout
  | EventTapDisabledByUserInput
  | EventKeyDown
  deriving (Eq, Show)

foreign import ccall unsafe kCGEventTapDisabledByTimeout_ :: IO CGEventType
kCGEventTapDisabledByTimeout :: CGEventType
kCGEventTapDisabledByTimeout = unsafePerformIO kCGEventTapDisabledByTimeout_
{-# NOINLINE kCGEventTapDisabledByTimeout #-}

foreign import ccall unsafe kCGEventTapDisabledByUserInput_ :: IO CGEventType
kCGEventTapDisabledByUserInput :: CGEventType
kCGEventTapDisabledByUserInput = unsafePerformIO kCGEventTapDisabledByUserInput_
{-# NOINLINE kCGEventTapDisabledByUserInput #-}
  
foreign import ccall unsafe kCGEventKeyDown_ :: IO CGEventType
kCGEventKeyDown :: CGEventType
kCGEventKeyDown = unsafePerformIO kCGEventKeyDown_
{-# NOINLINE kCGEventKeyDown #-}

toEventType :: CGEventType -> EventType
toEventType x
  | x == kCGEventTapDisabledByTimeout = EventTapDisabledByTimeout
  | x == kCGEventTapDisabledByUserInput = EventTapDisabledByUserInput
  | x == kCGEventKeyDown = EventKeyDown
  | otherwise = error "unknown CGEventType code"

foreign import ccall unsafe "mykCGEventFlagMaskSecondaryFn"
  kCGEventFlagMaskSecondaryFn :: IO CUInt

secondaryFn :: Word32
secondaryFn = fromIntegral (unsafePerformIO kCGEventFlagMaskSecondaryFn)
