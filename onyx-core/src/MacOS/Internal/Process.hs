{-# LANGUAGE RecordWildCards #-}

module MacOS.Internal.Process
  ( CarbonProcess(..)
  , carbonProcess
  -- * Query
  , processName
  , interactiveProcs
  , isProcessHidden
  -- * Events
  , installCarbonHandler
  ) where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign
import qualified MacOS.Internal.LLEvent as LL
import AppleSdk.Framework hiding (String)

data CarbonProcess = CProc
  { crbnPID :: PID
  , crbnPolicy :: ProcPolicy
  , crbnBackground :: Bool
  } deriving (Show)

-- | Name of the process.
processName :: CarbonProcess -> IO String
processName = getProcessName . crbnPID

isProcessHidden :: CarbonProcess -> IO (Maybe Bool)
isProcessHidden = isProcHidden . crbnPID

-- | Returns whether the process is interactive.
interactiveCarbonProc :: CarbonProcess -> Bool
interactiveCarbonProc CProc{..} = isProcessInteractive crbnBackground crbnPolicy

-- | Retrieve the Carbon process associated to a PSN.
carbonProcess :: PSN -> IO CarbonProcess
carbonProcess psn =
  CProc <$> processPID psn <*> processPolicy psn <*> getIsBackground psn

interactiveProcs :: IO [CarbonProcess]
interactiveProcs =
  filter interactiveCarbonProc <$> (allPSNs >>= mapM carbonProcess)

--------------------------------------------------------------------------------
-- Events

installCarbonHandler :: (LL.LLAppEvent -> IO ()) -> IO Bool
installCarbonHandler f = do
  target <- applicationEventTarget
  fptr <- wrap_carbon_callb (carbonHandler f)
  handler <- newEventHandlerUPP fptr
  p <-
    newArray
      [ EventTypeSpec eventClassApplication eventAppLaunched
      , EventTypeSpec eventClassApplication eventAppTerminated
      ]
  err <- installEventHandler target handler 2 p nullPtr nullPtr
  return $ err == 0

carbonHandler :: (LL.LLAppEvent -> IO ()) -> CarbonEventCallback
carbonHandler f _ e _ = alloca $ \p -> do
  err <- getEventParameter e eventParamProcessID typeProcessSerialNumber
           nullPtr (fromIntegral $ sizeOf nullPSN) nullPtr (castPtr p)
  if err == 0 then do
    ki <- getEventKind e
    psn <- peek p
    if ki == eventAppLaunched
      then (f (LL.AppLaunched psn)) >> pure 0
      else if ki == eventAppTerminated
        then (f (LL.AppTerminated psn)) >> pure 0
        else pure 0
  else pure err
