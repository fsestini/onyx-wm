module AppleSdk.Framework.CoreGraphics.Error where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))
import Control.Monad.Except

data CGError
  = CGErrorCannotComplete
  | CGErrorFailure
  | CGErrorIllegalArgument
  | CGErrorInvalidConnection
  | CGErrorInvalidContext
  | CGErrorInvalidOperation
  | CGErrorNoneAvailable
  | CGErrorNotImplemented
  | CGErrorRangeCheck
  | CGErrorTypeCheck
  | CGErrorUnknown
  deriving Show

data CGResult = CGErr CGError | CGSuccess

intToCGError :: CInt -> CGResult
intToCGError x
  | x == (#const kCGErrorCannotComplete) = CGErr CGErrorCannotComplete
  | x == (#const kCGErrorFailure) = CGErr CGErrorFailure
  | x == (#const kCGErrorIllegalArgument) = CGErr CGErrorIllegalArgument
  | x == (#const kCGErrorInvalidConnection) = CGErr CGErrorInvalidConnection
  | x == (#const kCGErrorInvalidContext) = CGErr CGErrorInvalidContext
  | x == (#const kCGErrorInvalidOperation) = CGErr CGErrorInvalidOperation
  | x == (#const kCGErrorNoneAvailable) = CGErr CGErrorNoneAvailable
  | x == (#const kCGErrorNotImplemented) = CGErr CGErrorNotImplemented
  | x == (#const kCGErrorRangeCheck) = CGErr CGErrorRangeCheck
  | x == (#const kCGErrorSuccess) = CGSuccess
  | x == (#const kCGErrorTypeCheck) = CGErr CGErrorTypeCheck
  | otherwise = CGErr CGErrorUnknown

type CG = ExceptT CGError IO

cgIO :: IO (Either CGError a) -> CG a
cgIO m = liftIO m >>= either throwError return

cgThrow :: CGError -> CG a
cgThrow = throwError

runCG :: CG a -> IO (Either CGError a)
runCG = runExceptT

cgEith :: CGResult -> Either CGError ()
cgEith (CGErr e) = Left e
cgEith CGSuccess = Right ()

cgEith' :: a -> CGResult -> Either CGError a
cgEith' _ (CGErr e) = Left e
cgEith' x CGSuccess = Right x
