module AppleSdk.Framework.Accessibility.Error
  ( module AppleSdk.Framework.Types
  , AXError(..)
  , AXResult
  , AX
  , ForeignAXError
  , toAXResult
  ) where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))
import AppleSdk.Framework.Types (Result(..), Action)

type ForeignAXError = CInt

toAXResult :: ForeignAXError -> AXResult
toAXResult e
  | e == (#const kAXErrorInvalidUIElementObserver) =
    Err AXErrorInvalidUIElementObserver
  | e == (#const kAXErrorIllegalArgument) = Err AXErrorIllegalArgument
  | e == (#const kAXErrorNotificationUnsupported) =
    Err AXErrorNotificationUnsupported
  | e == (#const kAXErrorNotificationAlreadyRegistered) =
    Err AXErrorNotificationAlreadyRegistered
  | e == (#const kAXErrorCannotComplete) = Err AXErrorCannotComplete
  | e == (#const kAXErrorFailure) = Err AXErrorFailure
  | e == (#const kAXErrorSuccess) = Ok
  | otherwise = Err AXErrorOther

--------------------------------------------------------------------------------
-- Accessibility monad

type AX = Action AXError

data AXError
  = AXErrorInvalidUIElementObserver
  | AXErrorIllegalArgument
  | AXErrorNotificationUnsupported
  | AXErrorNotificationAlreadyRegistered
  | AXErrorCannotComplete
  | AXErrorFailure
  | AXErrorOther
  deriving (Eq, Show)

type AXResult = Result AXError
