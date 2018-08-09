{-# LANGUAGE RecordWildCards #-}

module AppleSdk.Framework.Carbon.Process where

import AppleSdk.Framework.Carbon.Foreign
import AppleSdk.Framework.Carbon.PSN
import AppleSdk.Framework.Types
import Foreign

setFrontProcessFrontWindowOnly :: PID -> IO ()
setFrontProcessFrontWindowOnly pid = do
  psn <- processPSN pid
  alloca $ \p ->
    poke p (fromPSN psn) >> set_front_process_with_options p 1 >> pure ()

focusedProcess :: IO PSN
focusedProcess = alloca $ \psn -> get_front_process psn >> toPSN <$> peek psn

processPID :: PSN -> IO PID
processPID psn = alloca $ \p -> alloca $ \q ->
  poke p (fromPSN psn) >> get_process_pid p q >> peek q

processPSN :: PID -> IO PSN
processPSN pid = alloca $ \p -> get_process_for_pid pid p >> toPSN <$> peek p

processPolicy :: PSN -> IO ProcPolicy
processPolicy psn = do
  pid <- processPID psn
  alloca $ \p -> do
    _ <- proc_name_and_policy pid p
    fmap toPolicy $ peek p

allPSNs :: IO [PSN]
allPSNs = alloca $ \p -> poke p (PSN 0 0) >> nextAux p
  where
    nextAux p = get_next_process p >> aux p
    aux :: Ptr PSN -> IO [PSN]
    aux psn = do
      b <- fmap isNullPSN $ peek psn
      if b then return [] else (:) <$> peek psn <*> nextAux psn
