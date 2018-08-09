module AppleSdk.Framework.CoreGraphics.Error where

import Foreign.C.Types (CInt(..))
import System.IO.Unsafe
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
  deriving Show

data CGResult = CGErr CGError | CGSuccess

foreign import ccall unsafe cgErrorCannotComplete :: IO CInt
foreign import ccall unsafe cgErrorFailure :: IO CInt
foreign import ccall unsafe cgErrorIllegalArgument :: IO CInt
foreign import ccall unsafe cgErrorInvalidConnection :: IO CInt
foreign import ccall unsafe cgErrorInvalidContext :: IO CInt
foreign import ccall unsafe cgErrorInvalidOperation :: IO CInt
foreign import ccall unsafe cgErrorNoneAvailable :: IO CInt
foreign import ccall unsafe cgErrorNotImplemented :: IO CInt
foreign import ccall unsafe cgErrorRangeCheck :: IO CInt
foreign import ccall unsafe cgErrorSuccess :: IO CInt
foreign import ccall unsafe cgErrorTypeCheck :: IO CInt

intToCGError :: CInt -> CGResult
intToCGError x
  | x == unsafePerformIO cgErrorCannotComplete = CGErr CGErrorCannotComplete
  | x == unsafePerformIO cgErrorFailure = CGErr CGErrorFailure
  | x == unsafePerformIO cgErrorIllegalArgument = CGErr CGErrorIllegalArgument
  | x == unsafePerformIO cgErrorInvalidConnection = CGErr CGErrorInvalidConnection
  | x == unsafePerformIO cgErrorInvalidContext = CGErr CGErrorInvalidContext
  | x == unsafePerformIO cgErrorInvalidOperation = CGErr CGErrorInvalidOperation
  | x == unsafePerformIO cgErrorNoneAvailable = CGErr CGErrorNoneAvailable
  | x == unsafePerformIO cgErrorNotImplemented = CGErr CGErrorNotImplemented
  | x == unsafePerformIO cgErrorRangeCheck = CGErr CGErrorRangeCheck
  | x == unsafePerformIO cgErrorSuccess = CGSuccess
  | x == unsafePerformIO cgErrorTypeCheck = CGErr CGErrorTypeCheck
  | otherwise = error "unknown CGError code"

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
