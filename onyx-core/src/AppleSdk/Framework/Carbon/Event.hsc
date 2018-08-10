module AppleSdk.Framework.Carbon.Event
  ( EventTargetRef
  , EventHandlerProcPtr
  , EventHandlerUPP
  , EventHandlerRef
  , EventRef
  , HandlerCallRef
  , CarbonEventCallback
  , CarbonEvent -- (..)
  , EventParamName
  , EventParamType
  , eventParamProcessID
  , typeProcessSerialNumber
  , getEventParameter
  , installEventHandler
  , applicationEventTarget
  , wrap_carbon_callb
  , newEventHandlerUPP
  , eventClassApplication
  , eventAppLaunched
  , eventAppTerminated
  , getEventKind
  , getEventClass
  ) where

#include <Carbon/Carbon.h>

import Foreign.Ptr
import Data.Word
import AppleSdk.Framework.Types
import AppleSdk.Framework.Carbon.EventTypeSpec

data EventTarget
type EventTargetRef = Ptr EventTarget

data EventHandlerProc
type EventHandlerProcPtr = Ptr EventHandlerProc
type EventHandlerUPP = EventHandlerProcPtr

data EventHandler
type EventHandlerRef = Ptr EventHandler

data Event
type EventRef = Ptr Event

data HandlerCall
type HandlerCallRef = Ptr HandlerCall

foreign import ccall "InstallEventHandler"
  installEventHandler
  :: EventTargetRef -> EventHandlerUPP -> Word32
  -> Ptr EventTypeSpec -> Ptr () -> EventHandlerRef -> IO OSStatus

foreign import ccall "GetApplicationEventTarget"
  applicationEventTarget :: IO EventTargetRef

type CarbonEventCallback = HandlerCallRef -> EventRef -> Ptr () -> IO OSStatus

foreign import ccall "wrapper"
  wrap_carbon_callb :: CarbonEventCallback -> IO (FunPtr CarbonEventCallback)

foreign import ccall "handler_upp"
  newEventHandlerUPP :: FunPtr CarbonEventCallback -> IO EventHandlerUPP

eventClassApplication, eventAppLaunched, eventAppTerminated :: Word32
eventClassApplication = #const kEventClassApplication
eventAppLaunched = #const kEventAppLaunched
eventAppTerminated = #const kEventAppTerminated

foreign import ccall unsafe "GetEventKind" getEventKind :: EventRef -> IO Word32
foreign import ccall unsafe "GetEventClass" getEventClass :: EventRef -> IO Word32

data CarbonEvent

-- toCarbonEvent :: EventRef -> IO CarbonEvent
-- toCarbonEvent e = liftM2 aux (getEventClass e) (getEventKind e)
--   where
--     aux :: Word32 -> Word32 -> CarbonEvent
--     aux cl ki = undefined

type EventParamName = Word32
type EventParamType = Word32

foreign import ccall unsafe eventParamProcessID :: EventParamName
foreign import ccall unsafe "typeProcessSerialNumber_"
  typeProcessSerialNumber :: EventParamType

foreign import ccall unsafe "GetEventParameter" getEventParameter
  :: EventRef -> EventParamName -> EventParamType -> Ptr EventParamType
  -> Word32 -> Ptr Word32 -> Ptr () -> IO OSStatus
