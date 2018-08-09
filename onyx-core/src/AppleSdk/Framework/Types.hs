{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AppleSdk.Framework.Types
  ( PID
  , WindowID
  , DesktopID
  , OSStatus
  , OSErr
  , Result(..)
  , Action
  , IOIso(..)
  , Iso(..)
  , result
  , liftIO
  , action
  , resultAction
  , pureAction
  , runAction
  ) where

import Foreign
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans (liftIO)

type PID = Int32
type WindowID = Word32
type DesktopID = Int
type OSStatus = Int
type OSErr = Int

data Result e = Err e | Ok
  deriving (Eq, Ord, Show)

result :: (e -> a) -> a -> Result e -> a
result f _ (Err e) = f e
result _ x Ok = x

type Action e = ExceptT e IO

action :: IO (Either e a) -> Action e a
action = ExceptT

resultAction :: IO (Result e) -> Action e ()
resultAction = action . fmap isoTo

pureAction :: Either e a -> Action e a
pureAction = action . pure

runAction :: Action e a -> IO (Either e a)
runAction = runExceptT

instance Iso (Result e) (Maybe e) where
  isoTo = result Just Nothing
  isoFrom = maybe Ok Err

instance a ~ () => Iso (Result e) (Either e a) where
  isoTo = result Left (Right ())
  isoFrom = either Err (const Ok)

class IOIso a b where
  ioTo :: a -> IO b
  ioFrom :: b -> IO a

class Iso a b where
  isoTo :: a -> b
  isoFrom :: b -> a
