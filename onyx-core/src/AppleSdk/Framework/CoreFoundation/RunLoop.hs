module AppleSdk.Framework.CoreFoundation.RunLoop
  ( CFRunLoopSourceRef
  , RunLoopSource
  , runLoopContainsSource
  , runLoopAddSource
  , mainRunLoop
  , runLoopDefaultMode
  , runLoopSourceInvalidate
  , machPortRunLoopSource
  , runLoopCommonModes
  ) where

import Control.Monad.Managed
import AppleSdk.Framework.CoreFoundation.Object
import qualified AppleSdk.Framework.CoreFoundation.String as CF
import AppleSdk.Framework.CoreFoundation.MachPort
import AppleSdk.Framework.CoreFoundation.Allocator
import AppleSdk.Framework.CoreFoundation.Array
import Foreign hiding (with)
import Foreign.C.Types (CBool(..), CInt(..))

type RunLoopMode = CF.String
type CFRunLoopMode = CF.CFStringRef

data CFRunLoop
type CFRunLoopRef = Ptr CFRunLoop
instance CoreFoundationType CFRunLoop
newtype RunLoop = RunLoop (ForeignPtr CFRunLoop) deriving CFObject

data CFRunLoopSource
type CFRunLoopSourceRef = Ptr CFRunLoopSource
instance CoreFoundationType CFRunLoopSource
newtype RunLoopSource =
  RunLoopSource (ForeignPtr CFRunLoopSource)
  deriving (CFObject)

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopContainsSource"
  cfRunLoopContainsSource
  :: CFRunLoopRef -> CFRunLoopSourceRef -> CF.CFStringRef -> IO CBool

runLoopContainsSource :: RunLoop -> RunLoopSource -> CF.String -> IO Bool
runLoopContainsSource rl rls str = flip with pure $ do
  rl' <- managed $ withCFPtr rl
  rls' <- managed $ withCFPtr rls
  str' <- managed $ withCFPtr str
  liftIO . fmap toBool $ cfRunLoopContainsSource rl' rls' str'

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopAddSource"
  cfRunLoopAddSource
  :: CFRunLoopRef -> CFRunLoopSourceRef -> CF.CFStringRef -> IO ()

runLoopAddSource :: RunLoop -> RunLoopSource -> CF.String -> IO ()
runLoopAddSource rl rls str = runManaged $ do
  rl' <- managed $ withCFPtr rl
  rls' <- managed $ withCFPtr rls
  str' <- managed $ withCFPtr str
  liftIO $ cfRunLoopAddSource rl' rls' str'

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopGetMain"
  cfRunLoopGetMain :: IO CFRunLoopRef

mainRunLoop :: IO RunLoop
mainRunLoop = cfRunLoopGetMain >>= retainManageCFObj

foreign import ccall unsafe "kCFRunLoopDefaultMode_"
  kCFRunLoopDefaultMode :: IO CF.CFStringRef

runLoopDefaultMode :: IO CF.String
runLoopDefaultMode = kCFRunLoopDefaultMode >>= retainManageCFObj

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopSourceInvalidate"
  cfRunLoopSourceInvalidate :: CFRunLoopSourceRef -> IO ()

runLoopSourceInvalidate :: RunLoopSource -> IO ()
runLoopSourceInvalidate = flip withCFPtr cfRunLoopSourceInvalidate

foreign import ccall "CFMachPortCreateRunLoopSource"
  cfMachPortCreateRunLoopSource
  :: CFAllocatorRef -> CFMachPortRef -> CFIndex -> IO CFRunLoopSourceRef

machPortRunLoopSource :: Allocator -> MachPort -> Int -> IO RunLoopSource
machPortRunLoopSource al mp i = flip with pure $ do
  al' <- managed $ withCFPtr al
  mp' <- managed $ withCFPtr mp
  liftIO $ cfMachPortCreateRunLoopSource al' mp' (fromIntegral i) >>= manageCFObj

foreign import ccall unsafe "mykCFRunLoopCommonModes"
  cfRunLoopCommonModes :: IO CFRunLoopMode

runLoopCommonModes :: IO RunLoopMode
runLoopCommonModes = cfRunLoopCommonModes >>= fromCFPtr'
