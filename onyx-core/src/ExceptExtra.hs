{-# LANGUAGE TypeOperators #-}

module ExceptExtra
  ( MonadError(..)
  , ExceptT(..)
  , runExceptT
  , liftEither
  , logAndContinue
  ) where

import Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Except as E (throwError)
import Control.Monad.IO.Class (MonadIO(..))

class Monad m => MonadError e m where
  throwError :: e -> m a

instance (Monad m, e ~ e') => MonadError e (ExceptT e' m) where
  throwError = E.throwError

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure

logAndContinue :: (MonadIO m, Show e) => a -> ExceptT e m a -> m a
logAndContinue x a =
  runExceptT a >>= either (\e -> liftIO (print e) >> pure x) pure
