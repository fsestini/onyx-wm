module AppleSdk.Framework.CoreGraphics.Display where

import Data.Word
import Data.Bits
import Control.Monad
import AppleSdk.Framework.CoreFoundation
import AppleSdk.Framework.CoreGraphics.Error
import AppleSdk.Framework.CoreGraphics.Rect
import AppleSdk.Framework.CoreGraphics.Connection
import AppleSdk.Framework.Types
import Prelude hiding (String)
import Control.Monad.Managed
import Foreign.C.Types (CUInt(..), CInt(..))
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign hiding (with)

type DisplayID = Word32

foreign import ccall unsafe "CGDisplayCreateUUIDFromDisplayID"
  cgCreateUUIDFromDisplayID :: DisplayID -> IO CFUUIDRef

displayUUID :: DisplayID -> IO UUID
displayUUID = cgCreateUUIDFromDisplayID >=> manageCFObj

foreign import ccall unsafe "CFUUIDCreateString"
  cfUUIDCreateString :: CFAllocatorRef -> CFUUIDRef -> IO CFStringRef

uuidString :: Allocator -> UUID -> IO String
uuidString al uuid = flip with pure $ do
  al' <- managed $ withCFPtr al
  uuid' <- managed $ withCFPtr uuid
  liftIO $ cfUUIDCreateString al' uuid' >>= manageCFObj

uuidString' :: UUID -> IO String
uuidString' uuid = nullAllocator >>= flip uuidString uuid

foreign import ccall unsafe cgDisplayBounds_ :: CUInt -> Ptr Rect -> IO ()

displayBounds :: DisplayID -> IO Rect
displayBounds dId = alloca $ \p -> do
  cgDisplayBounds_ (fromIntegral dId) p
  peek p

type ForeignCGError = CInt

foreign import ccall unsafe "CGGetActiveDisplayList"
  cgGetActiveDisplayList
  :: CUInt -> Ptr DisplayID -> Ptr CUInt -> IO ForeignCGError

displayCount :: CG Int
displayCount = cgIO $ alloca $ \p -> do
  err <- cgGetActiveDisplayList 0 nullPtr p
  let res = intToCGError err
  case res of
    CGErr e -> pure $ Left e
    CGSuccess -> Right <$> (fmap fromIntegral $ peek p)

activeDisplays :: Int -> CG [DisplayID]
activeDisplays maxIds = cgIO $ allocaArray maxIds $ \p -> alloca $ \q -> do
  err <- cgGetActiveDisplayList (fromIntegral maxIds) p q
  let res = intToCGError err
  case res of
    CGErr e -> pure $ Left e
    CGSuccess -> do
      count <- fmap fromIntegral $ peek q
      Right <$> peekArray count p

--------------------------------------------------------------------------------

type DisplayChangeSummaryFlags = Word32

beginConfigFlag, movedFlag, setMainFlag, setModeFlag, addFlag, removeFlag,
  enabledFlag, disabledFlag, mirrorFlag, unmirrorFlag,
    desktopShapeChangedFlag :: DisplayChangeSummaryFlags
beginConfigFlag = 1
movedFlag = 1 `shiftL` 1
setMainFlag = 1 `shiftL` 2
setModeFlag = 1 `shiftL` 3
addFlag = 1 `shiftL` 4
removeFlag = 1 `shiftL` 5
enabledFlag = 1 `shiftL` 8
disabledFlag = 1 `shiftL` 9
mirrorFlag = 1 `shiftL` 10
unmirrorFlag = 1 `shiftL` 11
desktopShapeChangedFlag = 1 `shiftL` 12

data DisplayChange
  = DisplayBeginConfig
  | DisplayMoved
  | DisplaySetMain
  | DisplaySetMode
  | DisplayAdd
  | DisplayRemove
  | DisplayEnabled
  | DisplayDisabled
  | DisplayMirror
  | DisplayUnmirror
  | DisplayDesktopShapeChanged

toDisplayChange :: DisplayChangeSummaryFlags -> Maybe DisplayChange
toDisplayChange fl
  | Foreign.toBool (fl .&. beginConfigFlag) = Just DisplayBeginConfig
  | Foreign.toBool (fl .&. movedFlag) = Just DisplayMoved
  | Foreign.toBool (fl .&. setMainFlag) = Just DisplaySetMain
  | Foreign.toBool (fl .&. setModeFlag) = Just DisplaySetMode
  | Foreign.toBool (fl .&. addFlag) = Just DisplayAdd
  | Foreign.toBool (fl .&. removeFlag) = Just DisplayRemove
  | Foreign.toBool (fl .&. enabledFlag) = Just DisplayEnabled
  | Foreign.toBool (fl .&. disabledFlag) = Just DisplayDisabled
  | Foreign.toBool (fl .&. mirrorFlag) = Just DisplayMirror
  | Foreign.toBool (fl .&. unmirrorFlag) = Just DisplayUnmirror
  | Foreign.toBool (fl .&. desktopShapeChangedFlag) = Just DisplayDesktopShapeChanged
  | otherwise = Nothing

--------------------------------------------------------------------------------

type CGDisplayReconfigurationCallback =
  DisplayID -> DisplayChangeSummaryFlags -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper" wrap_display_callback
  :: CGDisplayReconfigurationCallback -> IO (FunPtr CGDisplayReconfigurationCallback)

type DisplayReconfigurationCallback = DisplayID -> DisplayChange -> IO ()

toCGCallback :: DisplayReconfigurationCallback -> CGDisplayReconfigurationCallback
toCGCallback cb did flags _ = maybe (pure ()) (cb did) (toDisplayChange flags)

foreign import ccall "CGDisplayRegisterReconfigurationCallback"
  cgDisplayRegisterReconfigurationCallback_
  :: FunPtr CGDisplayReconfigurationCallback -> Ptr () -> IO ForeignCGError

setDisplayCallback :: DisplayReconfigurationCallback -> IO CGResult
setDisplayCallback f = do
  fp <- wrap_display_callback (toCGCallback f)
  fmap intToCGError $ cgDisplayRegisterReconfigurationCallback_ fp nullPtr

foreign import ccall unsafe "CGSCopyManagedDisplayForWindow"
  cgSCopyManagedDisplayForWindow_
  :: ConnectionID -> CUInt -> IO CFStringRef

displayForWindow :: ConnectionID -> WindowID -> IO String
displayForWindow cid wid =
  cgSCopyManagedDisplayForWindow_ cid (fromIntegral wid) >>= manageCFObj

--------------------------------------------------------------------------------

foreign import ccall unsafe display_for_window :: CUInt -> IO CFStringRef

displayForWindow' :: WindowID -> IO String
displayForWindow' = manageCFObj <=< display_for_window . fromIntegral

--------------------------------------------------------------------------------

foreign import ccall unsafe activeDisplay :: IO DisplayID
