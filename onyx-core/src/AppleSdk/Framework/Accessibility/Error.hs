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

import Foreign.C.Types (CInt(..))
import AppleSdk.Framework.Types (Result(..), Action)
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans
-- import Control.Monad
-- import Foreign (Storable, Ptr, alloca, mallocForeignPtr, withForeignPtr)

type ForeignAXError = CInt

foreign import ccall error_invalid_element_observer :: ForeignAXError
foreign import ccall error_illegal_argument :: ForeignAXError
foreign import ccall error_notification_unsupported :: ForeignAXError
foreign import ccall error_notification_already_registered :: ForeignAXError
foreign import ccall error_cannot_complete :: ForeignAXError
foreign import ccall error_failure :: ForeignAXError
foreign import ccall error_success :: ForeignAXError

toAXResult :: ForeignAXError -> AXResult
toAXResult e
  | e == error_invalid_element_observer = Err AXErrorInvalidUIElementObserver
  | e == error_illegal_argument = Err AXErrorIllegalArgument
  | e == error_notification_unsupported = Err AXErrorNotificationUnsupported
  | e == error_notification_already_registered =
    Err AXErrorNotificationAlreadyRegistered
  | e == error_cannot_complete = Err AXErrorCannotComplete
  | e == error_failure = Err AXErrorFailure
  | e == error_success = Ok
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
