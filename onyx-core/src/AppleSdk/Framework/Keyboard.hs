module AppleSdk.Framework.Keyboard
  ( Modifier(..)
  , ModifierPos(..)
  , Key(..)
  , ModifierPress
  , KeyPress(..)
  , Keycode
  , KeyFlags
  , KeyCallback
  , setKeyboardCallback
  , keycodeFromChar
  , toKeycode
  , toModPress
  , initKeycodeMap
  ) where

import AppleSdk.Framework.CoreGraphics.Event
import Control.Monad.State
import Data.Bits
import qualified Data.Set as S
import Data.Word
import Foreign.C.Types (CUInt(..), CBool(..))
import Foreign
import Prelude hiding (String)

data Modifier = Alt | Ctrl | Cmd | Shift | Fn deriving (Eq, Ord, Show)
data ModifierPos = Neutral | LeftPos | RightPos deriving (Eq, Ord, Show)

data Key
  = KeyLiteral Char
  | KeySpace
  | KeyReturn
  | KeyEsc
  | KeyArrowUp
  | KeyArrowDown
  | KeyArrowRight
  | KeyArrowLeft
  deriving (Eq, Ord, Show)

type ModifierPress = (Modifier, ModifierPos)

data KeyPress = KPress
  { kpKey :: Keycode
  , kpMod :: S.Set ModifierPress
  } deriving (Eq, Show)

type Keycode = Word32

toKeycode :: Key -> Keycode
toKeycode KeySpace = 0x31
toKeycode KeyReturn = 0x24
toKeycode KeyEsc = 0x35
toKeycode KeyArrowUp = 0x7E
toKeycode KeyArrowDown = 0x7D
toKeycode KeyArrowRight = 0x7C
toKeycode KeyArrowLeft = 0x7B
toKeycode (KeyLiteral c) = keycodeFromChar c

data OSXEventMask
  = EM_Alt
  | EM_LAlt
  | EM_RAlt
  | EM_Shift
  | EM_LShift
  | EM_RShift
  | EM_Cmd
  | EM_LCmd
  | EM_RCmd
  | EM_Control
  | EM_LControl
  | EM_RControl
  | EM_Fn
  deriving (Eq, Show)

emToUInt32 :: OSXEventMask -> Word32
emToUInt32 EM_Alt      = 0x00080000
emToUInt32 EM_LAlt     = 0x00000020
emToUInt32 EM_RAlt     = 0x00000040
emToUInt32 EM_Shift    = 0x00020000
emToUInt32 EM_LShift   = 0x00000002
emToUInt32 EM_RShift   = 0x00000004
emToUInt32 EM_Cmd      = 0x00100000
emToUInt32 EM_LCmd     = 0x00000008
emToUInt32 EM_RCmd     = 0x00000010
emToUInt32 EM_Control  = 0x00040000
emToUInt32 EM_LControl = 0x00000001
emToUInt32 EM_RControl = 0x00002000
emToUInt32 EM_Fn       = secondaryFn

type KeyFlags = Word32

cgevent_lrmod_flag :: [[OSXEventMask]]
cgevent_lrmod_flag =
  [ [EM_Alt , EM_LAlt , EM_RAlt] , [EM_Shift , EM_LShift , EM_RShift]
  , [EM_Cmd , EM_LCmd , EM_RCmd] , [EM_Control , EM_LControl , EM_RControl] ]

onlyIf :: Applicative m => Bool -> m () -> m ()
onlyIf True x = x
onlyIf False _ = pure ()

addFlags :: ModifierPress -> State [ModifierPress] ()
addFlags p = modify (p :)

lrToHotKey :: KeyFlags -> Modifier -> State [ModifierPress] ()
lrToHotKey flags modif =
  onlyIf ((flags .&. mask) == mask) $ do
    onlyIf left $ addFlags (modif, LeftPos)
    onlyIf right $ addFlags (modif, RightPos)
    onlyIf (not left && not right) $ addFlags (modif, Neutral)
  where
    mask = emToUInt32 (cgevent_lrmod_flag !! modCode modif !! 0)
    lmask = emToUInt32 (cgevent_lrmod_flag !! modCode modif !! 1)
    rmask = emToUInt32 (cgevent_lrmod_flag !! modCode modif !! 2)
    left = (flags .&. lmask) == lmask ; right = (flags .&. rmask) == rmask
    modCode :: Modifier -> Int
    modCode Alt = 0
    modCode Shift = 1
    modCode Cmd = 2
    modCode Ctrl = 3
    modCode Fn = 4

toHotKeyFlags :: KeyFlags -> State [ModifierPress] ()
toHotKeyFlags flags = do
  lrToHotKey flags Alt
  lrToHotKey flags Shift
  lrToHotKey flags Cmd
  lrToHotKey flags Ctrl

  onlyIf (flags .&. fncode == fncode) $ addFlags (Fn , Neutral)
  where fncode = emToUInt32 EM_Fn

toModPress :: KeyFlags -> [ModifierPress]
toModPress x = snd $ runState (toHotKeyFlags x) []

--------------------------------------------------------------------------------

type KeyCallback = KeyFlags -> Keycode -> IO Bool
type RawCallback = CUInt -> CUInt -> IO CBool

foreign import ccall set_key_callback :: FunPtr RawCallback -> IO ()
foreign import ccall "wrapper"
  wrap_key_callback :: RawCallback -> IO (FunPtr RawCallback)

setKeyboardCallback :: KeyCallback -> IO ()
setKeyboardCallback callb = do
  f <- wrap_key_callback $ \x y ->
    fmap fromBool $ callb (fromIntegral x) (fromIntegral y)
  set_key_callback f

foreign import ccall "keycode_from_char" keycodeFromChar :: Char -> Keycode

foreign import ccall "initialize_keycode_map" init_keycode_map :: IO CBool

initKeycodeMap :: IO Bool
initKeycodeMap = fmap toBool init_keycode_map
