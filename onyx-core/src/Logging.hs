module Logging where

import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad.Trans

log' :: MonadIO m => String -> m ()
log' str = liftIO getCurrentTime >>= \t ->
  let (TimeOfDay h m _) = timeToTimeOfDay . utctDayTime $ t
      d = utctDay t
  in liftIO $ putStrLn (show d ++ " " ++ show h ++ ":" ++ show m ++ ": " ++ str)

log'' :: (MonadIO m, Show a) => a -> m ()
log'' = log' . show
