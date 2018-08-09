module AppleSdk.Framework.Accessibility.Observer
  ( Observer
  , ObserverCallback
  , observerCreate
  , observerGetRunLoopSource
  -- * Notifications
  , addNotification
  , removeNotification
  ) where

import Control.Monad.Managed
import AppleSdk.Framework.Types
       (PID, Iso(..), liftIO, resultAction, action)
import AppleSdk.Framework.Accessibility.Error
       (ForeignAXError, AX, AXResult, toAXResult)
import AppleSdk.Framework.Accessibility.UIElement
import AppleSdk.Framework.Accessibility.UINotification
import AppleSdk.Framework.CoreFoundation
import Foreign hiding (with)
import Foreign.C.Types (CInt(..))
import Prelude hiding (String)

data AXObserver
type AXObserverRef = Ptr AXObserver
newtype Observer = Observer { getObserver :: ForeignPtr AXObserver }

instance CoreFoundationType AXObserver
instance CFObject Observer where
  type CFT Observer = AXObserver
  toCFPtr = getObserver
  fromCFPtr = Observer

foreign import ccall "wrapper"
  wrap_observer_callback :: AXObserverCallback -> IO (FunPtr AXObserverCallback)

foreign import ccall "Carbon/Carbon.h AXObserverCreate"
  ax_observer_create
  :: PID -> FunPtr AXObserverCallback -> Ptr AXObserverRef -> IO ForeignAXError

type AXObserverCallback =
  AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr () -> IO ()

type ObserverCallback =
  Observer -> UIElement -> UINotification -> StablePtr () -> IO ()

toAXCallback :: ObserverCallback -> AXObserverCallback
toAXCallback f obs el str ptr = do
  obs' <- retainManageCFObj obs
  el'  <- retainManageCFObj el
  mNotif <- toNotif str
  maybe (pure ()) (\notif -> f obs' el' notif ptr) mNotif

observerCreate :: PID -> ObserverCallback -> AX Observer
observerCreate pid f =
  (liftIO (wrap_observer_callback (toAXCallback f)) >>= axObserverCreate pid)
    >>= liftIO . manageCFObj

axObserverCreate :: PID -> FunPtr AXObserverCallback -> AX AXObserverRef
axObserverCreate pid callb = action $ alloca $ \p -> do
  err <- ax_observer_create pid callb p
  o <- peek p
  pure $ isoTo (toAXResult err) >> pure o

foreign import ccall unsafe "Carbon/Carbon.h AXObserverGetRunLoopSource"
  axObserverGetRunLoopSource :: AXObserverRef -> IO CFRunLoopSourceRef

observerGetRunLoopSource :: Observer -> IO RunLoopSource
observerGetRunLoopSource obs = withCFPtr obs $ \optr ->
  axObserverGetRunLoopSource optr >>= retainManageCFObj

-- foreign import ccall start_observer :: AXObserverRef -> IO ()

-- startObserver :: Observer -> IO ()
-- startObserver = flip withCFPtr start_observer

-- foreign import ccall unsafe "Carbon/Carbon.h _AXUIElementGetWindow"
--   ax_ui_element_get_window :: AXUIElementRef -> Ptr CUInt -> IO ForeignAXError

-- getWindow :: UIElement -> AX WindowID
-- getWindow uiel = action $ withCFPtr uiel $ \el -> alloca $ \p -> do
--   err <- ax_ui_element_get_window el p
--   wid <- peek p
--   pure $ isoTo (toAXResult err) >> pure (fromIntegral wid)

foreign import ccall unsafe "Carbon/Carbon.h AXObserverAddNotification"
  ax_observer_add_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr a
  -> IO ForeignAXError

axObserverAddNotification
  :: AXObserverRef -> AXUIElementRef -> UINotification -> StablePtr a -> IO AXResult
axObserverAddNotification obs el notif ptr = do
  str <- notifString notif
  fmap toAXResult (ax_observer_add_notification obs el str ptr)

-- axObserverAddNotification'
--   :: AXObserverRef -> AXUIElementRef -> UINotification -> StablePtr a -> AX ()
-- axObserverAddNotification' obs el notif ptr =
--   resultAction $ axObserverAddNotification obs el notif ptr

addNotification :: Observer -> UIElement -> UINotification -> StablePtr a -> AX ()
addNotification obs el notif ptr = resultAction $ flip with pure $ do
  obs' <- managed $ withCFPtr obs
  el'  <- managed $ withCFPtr el
  liftIO $ axObserverAddNotification obs' el' notif ptr

foreign import ccall "Carbon/Carbon.h AXObserverRemoveNotification"
  ax_observer_remove_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> IO ForeignAXError

axObserverRemoveNotification
  :: AXObserverRef -> AXUIElementRef -> UINotification -> IO AXResult
axObserverRemoveNotification obs el notif = do
  str <- notifString notif
  fmap toAXResult (ax_observer_remove_notification obs el str)

-- axObserverRemoveNotification'
--   :: AXObserverRef -> AXUIElementRef -> UINotification -> AX ()
-- axObserverRemoveNotification' obs el notif =
--   resultAction $ axObserverRemoveNotification obs el notif

removeNotification :: Observer -> UIElement -> UINotification -> AX ()
removeNotification obs el notif = resultAction . flip with pure $ do
  obs' <- managed $ withCFPtr obs
  el'  <- managed $ withCFPtr el
  liftIO $ axObserverRemoveNotification obs' el' notif
