-- | MacOS process management types and functions.

module MacOS.Process
  ( CarbonProcess
  , PID
  , PSN
  , processName
  -- , interactiveCarbonProc
  -- , carbonProcess
  , installCarbonHandler
  , interactiveProcs
  , processPID
  , setFrontProcessFrontWindowOnly
  , focusedProcess
  ) where

import AppleSdk
       (PID, PSN, processPID, setFrontProcessFrontWindowOnly,
        focusedProcess)
import MacOS.Internal.Process

