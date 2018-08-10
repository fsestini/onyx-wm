{-# LANGUAGE LambdaCase #-}

module AppleSdk.Framework.Accessibility.Error
  ( module AppleSdk.Framework.Types
  , AXError(..)
  , AXResult
  , AX
  , ForeignAXError
  , toAXResult
  -- , allocAX
  -- , mallocAX
  -- , throwAwayAX
  -- , eToM
  -- , axToMay
  -- , injAXResult
  ) where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))
import AppleSdk.Framework.Types (Result(..), Action)
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans
-- import Control.Monad
-- import Foreign (Storable, Ptr, alloca, mallocForeignPtr, withForeignPtr)

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

-- toForeignAXError :: AXResult  -> ForeignAXError
-- toForeignAXError (AXErr AXErrorInvalidUIElementObserver) =
--   error_invalid_element_observer
-- toForeignAXError (AXErr AXErrorIllegalArgument) = error_illegal_argument
-- toForeignAXError (AXErr AXErrorNotificationUnsupported) =
--   error_notification_unsupported
-- toForeignAXError (AXErr AXErrorNotificationAlreadyRegistered) =
--   error_notification_already_registered
-- toForeignAXError (AXErr AXErrorCannotComplete) = error_cannot_complete
-- toForeignAXError (AXErr AXErrorFailure) = error_failure
-- toForeignAXError AXSuccess = error_success

--------------------------------------------------------------------------------
-- Accessibility monad

-- type ExceptIO e = ExceptT e IO
type AX = Action AXError

-- allocAX :: Storable a => (Ptr a -> AX b) -> AX b
-- allocAX f = axErrorIO . alloca $ runAXError . f

-- mallocAX :: Storable a => (Ptr a -> AX b) -> AX b
-- mallocAX f = axErrorIO $ do
--   fp <- mallocForeignPtr
--   withForeignPtr fp $ runAXError . f

-- axErrorIO :: IO (Either AXError a) -> AX a
-- axErrorIO = ExceptT

-- runAXError :: AX a -> IO (Either AXError a)
-- runAXError = runExceptT

-- throwAwayAX :: AX () -> IO ()
-- throwAwayAX = void . runAXError

-- eToM :: Either e a -> Maybe a
-- eToM = either (const Nothing) Just

-- axToMay :: AX a -> IO (Maybe a)
-- axToMay = fmap eToM . runAXError

-- toEitherErr :: a -> AXResult -> Either AXError a
-- toEitherErr x Ok = Right x
-- toEitherErr _ (Err e) = Left e

-- injAXResult :: IO AXResult -> AX ()
-- injAXResult m = liftIO m >>= \case
--   Ok -> pure ()
--   Err e   -> throwE e

-- ioRes :: AX () -> IO AXResult
-- ioRes = fmap isoFrom . runAXError

-- catchAXErr :: AX a -> (AXError -> AX a) -> AX a
-- catchAXErr = catchE

-- ioErr :: IO a -> AX a
-- ioErr = liftIO

-- exceptErr :: Either AXError a -> AX a
-- exceptErr = ExceptT . pure

-- ioEither :: IO (Either AXError a) -> AX a
-- ioEither = join . ioErr . fmap exceptErr

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

-- axResult :: (AXError -> a) -> a -> AXResult -> a
-- axResult f _ (AXErr e) = f e
-- axResult _ x AXSuccess = x

-- eitherErr :: Either AXError () -> AXResult
-- eitherErr (Left e) = AXErr e
-- eitherErr (Right ()) = AXSuccess
