{-# LANGUAGE TemplateHaskell #-}

-- | 

module MacOS.Internal.Type where

import qualified MacOS.Internal.LLEvent as LL (newQueue)
import MacOS.Internal.Event (EventQueue)
import MacOS.Internal.Window (Window(..))
import MacOS.Internal.App (App(..))

import AppleSdk
       (WindowID, PID, Action, runAction, checkAXPrivileges, nsAppLoad,
        setMessaging, CGError, AXError)

import Control.Monad (void, join)
import Control.Monad.Reader (ReaderT, ask,runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import ExceptExtra
import Control.Monad.Trans (lift)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO, newTVarIO)
import Data.Bifunctor (first)
import Data.List (find)
import Lens.Micro.Platform (makeLenses, over, Getting, (^.))

data MacState = MacState { _stWins :: [Window], _stApps :: [App] }
data MacConfig = MacConfig
  { _queue :: EventQueue
  , _macState :: TVar MacState
  }
makeLenses ''MacState
makeLenses ''MacConfig

class MonadIO m => MonadMac m where
  askMac :: m MacConfig

instance MonadMac m => MonadMac (ExceptT e m) where
  askMac = lift askMac

instance MonadMac m => MonadMac (ReaderT e m) where
  askMac = lift askMac

instance MonadMac m => MonadMac (StateT e m) where
  askMac = lift askMac

newtype Mac a = Mac { unMac :: ReaderT MacConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadMac Mac where
  askMac = Mac ask

viewMac :: MonadMac m => Getting a MacConfig a -> m a
viewMac l = fmap (\c -> c ^. l) askMac

data MacError
  = CacheError CacheError
  | HandlerError HandlerError
  | UnspecifiedError
  | TimeoutError
  | CGError CGError
  | AXError AXError
  deriving Show

data CacheError
  = WindowNotCached WindowID
  | AppNotCached PID
  deriving (Show)

data HandlerError
  = CarbonHandlerError
  | WorkspaceHandlerError
  | DisplayHandlerError
  | KeyHandlerError
  deriving Show

apps :: MonadMac m => m [App]
apps = askMac >>= liftIO . fmap _stApps . readTVarIO . _macState

wins :: MonadMac m => m [Window]
wins = askMac >>= liftIO . fmap _stWins . readTVarIO . _macState

overState :: MonadMac m => (MacState -> MacState) -> m ()
overState f = fmap _macState askMac >>= liftIO . atomically . flip modifyTVar' f

overApps :: MonadMac m => ([App] -> [App]) -> m ()
overApps = overState . over stApps

overWins :: MonadMac m => ([Window] -> [Window]) -> m ()
overWins = overState . over stWins

findByPID :: (MonadMac m, MonadError MacError m) => PID -> m App
findByPID pid = do
  m <- askMac >>=
    liftIO . fmap (find ((== pid) . _appPID) . _stApps) . readTVarIO . _macState
  maybe (throwError (CacheError (AppNotCached pid))) pure m

findByWID :: (MonadMac m, MonadError MacError m) => WindowID -> m Window
findByWID wid = do
  m <- askMac >>=
    liftIO . fmap (find ((== wid) . _windowID) . _stWins) . readTVarIO . _macState
  maybe (throwError (CacheError (WindowNotCached wid))) pure m

injAction :: (MonadError e' m, MonadIO m) => (e -> e') -> Action e a -> m a
injAction f = join . liftIO . fmap (liftEither . first f) . runAction

isAppCached :: MonadMac m => PID -> m Bool
isAppCached = fmap (either (const False) (const True)) . runExceptT . findByPID

isWinCached :: MonadMac m => WindowID -> m Bool
isWinCached = fmap (either (const False) (const True)) . runExceptT . findByWID

logIO :: (Show a, MonadIO m) => String -> m a -> m ()
logIO s m = do
  liftIO (putStr s)
  x <- m
  liftIO (print x)

runMac :: Mac a -> IO a
runMac m = do
  q <- LL.newQueue
  v <- newTVarIO (MacState [] [])
  runReaderT (unMac m) (MacConfig q v)

macInitIO :: IO ()
macInitIO = do
  logIO "Checking AX privileges: " $ checkAXPrivileges >>= print
  logIO "NS App loading: " $ nsAppLoad >> (void . runAction) setMessaging
  putStrLn "Internal NS initialization completed"
