module AppleSdk.Framework.Cocoa
  ( checkAXPrivileges
  , setSignal
  , nsAppLoad
  , setMessaging
  , runLoop
  ) where

import AppleSdk.Framework.Accessibility
import AppleSdk.Framework.CoreFoundation
import AppleSdk.Framework.Types (liftIO)
import Foreign
import Foreign.C.Types (CBool(..))
import Prelude hiding (String)

foreign import ccall unsafe axTrustedCheckOptionPrompt :: IO CFStringRef

trustedCheckOptionPrompt :: IO String
trustedCheckOptionPrompt = axTrustedCheckOptionPrompt >>= manageCFObj

foreign import ccall unsafe "AXIsProcessTrustedWithOptions"
  axIsProcessTrustedWithOptions :: Ptr () -> IO CBool

isProcessTrustedWithOptions :: Dictionary -> IO Bool
isProcessTrustedWithOptions =
  fmap Foreign.toBool . flip withCFPtr (axIsProcessTrustedWithOptions . castPtr)

checkAXPrivileges :: IO Bool
checkAXPrivileges = do
  allo <- nullAllocator
  s <- trustedCheckOptionPrompt
  b <- booleanTrue
  createDictionary allo [s] [b] >>= isProcessTrustedWithOptions

foreign import ccall
  set_signal :: IO ()

foreign import ccall "NSApplicationLoad" ns_app_load :: IO ()

setMessaging :: AX ()
setMessaging = liftIO systemWideUIElement >>= flip setMessagingTimeout 1

foreign import ccall "CFRunLoopRun" run_loop :: IO ()

-- checkAXPrivileges :: IO Bool
-- checkAXPrivileges = fmap Foreign.toBool check_ax_privileges

setSignal :: IO ()
setSignal = set_signal

nsAppLoad :: IO ()
nsAppLoad = ns_app_load

runLoop :: IO ()
runLoop = run_loop
