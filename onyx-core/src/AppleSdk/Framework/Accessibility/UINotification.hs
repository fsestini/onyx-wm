module AppleSdk.Framework.Accessibility.UINotification where

import AppleSdk.Framework.CoreFoundation
import Control.Monad.Except

data UINotification
  = WindowCreatedNotification
  | FocusedWindowChangedNotification
  | WindowMovedNotification
  | WindowResizedNotification
  | TitleChangedNotification
  | UIElementDestroyedNotification
  | WindowMiniaturizedNotification
  | WindowDeminiaturizedNotification
  deriving (Eq, Show)

foreign import ccall window_created_notif :: IO CFStringRef
foreign import ccall focused_window_changed_notif :: IO CFStringRef
foreign import ccall window_moved_notif :: IO CFStringRef
foreign import ccall window_resized_notif :: IO CFStringRef
foreign import ccall title_changed_notif :: IO CFStringRef
foreign import ccall ax_ui_element_destroyed_notification :: IO CFStringRef
foreign import ccall ax_window_miniaturized_notification :: IO CFStringRef
foreign import ccall ax_window_deminiaturized_notification :: IO CFStringRef

ifM :: Monad m => Bool -> a -> ExceptT a m ()
ifM b x = if b then throwError x else pure ()

toNotif' :: CFStringRef -> ExceptT UINotification IO ()
toNotif' str = do
  wcn <- liftIO window_created_notif
  b_wcn <- liftIO $ cfEqual str wcn
  ifM b_wcn WindowCreatedNotification

  fwc <- liftIO focused_window_changed_notif
  b_fwc <- liftIO $ cfEqual str fwc
  ifM b_fwc FocusedWindowChangedNotification

  wm <- liftIO window_moved_notif
  b_wm <- liftIO $ cfEqual str wm
  ifM b_wm WindowMovedNotification

  wr <- liftIO window_resized_notif
  b_wr <- liftIO $ cfEqual str wr
  ifM b_wr WindowResizedNotification

  tc <- liftIO title_changed_notif
  b_tc <- liftIO $ cfEqual str tc
  ifM b_tc TitleChangedNotification

  ed <- liftIO ax_ui_element_destroyed_notification
  b_ed <- liftIO $ cfEqual str ed
  ifM b_ed UIElementDestroyedNotification

  wmin <- liftIO ax_window_miniaturized_notification
  b_wmin <- liftIO $ cfEqual str wmin
  ifM b_wmin WindowMiniaturizedNotification

  wdemin <- liftIO ax_window_deminiaturized_notification
  b_wdemin <- liftIO $ cfEqual str wdemin
  ifM b_wdemin WindowDeminiaturizedNotification

toNotif :: CFStringRef -> IO (Maybe UINotification)
toNotif = fmap (either Just (const Nothing)) . runExceptT . toNotif'

notifString :: UINotification -> IO CFStringRef
notifString WindowCreatedNotification = window_created_notif
notifString FocusedWindowChangedNotification = focused_window_changed_notif
notifString WindowMovedNotification = window_moved_notif
notifString WindowResizedNotification = window_resized_notif
notifString TitleChangedNotification = title_changed_notif
notifString UIElementDestroyedNotification = ax_ui_element_destroyed_notification
notifString WindowMiniaturizedNotification = ax_window_miniaturized_notification
notifString WindowDeminiaturizedNotification = ax_window_deminiaturized_notification
