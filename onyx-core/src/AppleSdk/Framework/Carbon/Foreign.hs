{-# LANGUAGE ForeignFunctionInterface #-}

module AppleSdk.Framework.Carbon.Foreign where

import Foreign
import Foreign.Marshal.Alloc (free)
import Foreign.C.Types (CInt(..), CUInt(..), CBool(..))
import Foreign.C.String
import AppleSdk.Framework.Carbon.PSN
import AppleSdk.Framework.Types

type ForeignProcPolicy = Word32
data ProcPolicy
  = PolicyRegular
  | PolicyUIElement
  | PolicyBackground
  | PolicyUnrecognized ForeignProcPolicy
  deriving (Eq, Show)

toPolicy :: ForeignProcPolicy -> ProcPolicy
toPolicy p | p == 0 = PolicyRegular
           | p == 1 = PolicyUIElement
           | p == 2 = PolicyBackground
           | otherwise = PolicyUnrecognized p

fromPolicy :: ProcPolicy -> ForeignProcPolicy
fromPolicy PolicyRegular = 0
fromPolicy PolicyUIElement = 1
fromPolicy PolicyBackground = 2
fromPolicy (PolicyUnrecognized p) = p

isProcessInteractive :: Bool -> ProcPolicy -> Bool
isProcessInteractive isBackgr p = not isBackgr && p == PolicyRegular

emptyPSN :: PSN -> Bool
emptyPSN (PSN x y) = x == 0 && y == 0

foreign import ccall unsafe "GetNextProcess"
  get_next_process :: Ptr PSN -> IO OSErr
foreign import ccall unsafe get_isback :: Ptr PSN -> IO CBool

withPtr :: Storable a => (Ptr a -> IO b) -> a -> IO b
withPtr f x = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \p -> poke p x >> f p

getIsBackground :: PSN -> IO Bool
getIsBackground = fmap toBool . withPtr get_isback . fromPSN

foreign import ccall unsafe
  is_proc_hidden :: PID -> Ptr CBool -> IO CBool

isProcHidden :: PID -> IO (Maybe Bool)
isProcHidden pid = flip withPtr (fromBool False) $ \p -> do
  b <- is_proc_hidden pid p
  if toBool b then fmap (Just . toBool) (peek p) else pure Nothing

foreign import ccall unsafe
  proc_name_and_policy :: PID -> Ptr ForeignProcPolicy -> IO CString

type ForeignOSStatus = CInt

isNullPSN :: PSN -> Bool
isNullPSN (PSN x y) = x == 0 && y == 0

toPSN :: PSN -> PSN
toPSN (PSN high low) = PSN (fromIntegral high) (fromIntegral low)

fromPSN :: PSN -> PSN
fromPSN (PSN high low) = PSN (fromIntegral high) (fromIntegral low)

foreign import ccall "Carbon/Carbon.h SetFrontProcessWithOptions"
  set_front_process_with_options :: Ptr PSN -> CUInt -> IO OSStatus

foreign import ccall "Carbon/Carbon.h GetProcessForPID"
  get_process_for_pid :: PID -> Ptr PSN -> IO OSStatus

foreign import ccall "Carbon/Carbon.h GetProcessPID"
  get_process_pid :: Ptr PSN -> Ptr PID -> IO OSStatus

foreign import ccall "Carbon/Carbon.h GetFrontProcess"
  get_front_process :: Ptr PSN -> IO OSErr

getProcessName :: PID -> IO String
getProcessName pid = alloca $ \p -> do
  str <- proc_name_and_policy pid p
  if str == nullPtr
     then return "<unknown>"
     else peekCString str <* free str
