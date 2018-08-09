-- | Keyboard management types and functions.

module MacOS.Keyboard
  ( ModifierPress
  , Modifier(..)
  , ModifierPos(..)
  , KeyPress(..)
  , Key(..)
  , toKeycode
  , installKeyboardHandler
  ) where

import AppleSdk
       (KeyPress(..), Key(..), ModifierPos(..), Modifier(..),
        ModifierPress, toKeycode)
import MacOS.Internal.Keyboard(installKeyboardHandler)
