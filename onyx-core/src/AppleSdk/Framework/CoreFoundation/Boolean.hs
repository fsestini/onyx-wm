{-# LANGUAGE TypeSynonymInstances #-}

module AppleSdk.Framework.CoreFoundation.Boolean
  ( Boolean
  , CFBooleanRef
  -- , AppleSdk.Framework.CoreFoundation.Boolean.toBool
  -- , AppleSdk.Framework.CoreFoundation.Boolean.fromBool
  , booleanFalse
  , booleanTrue
  ) where

import AppleSdk.Framework.CoreFoundation.Object
import qualified Foreign.C.Types as C (CChar(..))
import AppleSdk.Framework.Types
import Foreign
import Data.Bool

data CFBoolean
type CFBooleanRef = Ptr CFBoolean
instance CoreFoundationType CFBoolean
newtype Boolean = Boolean (ForeignPtr CFBoolean) deriving (CFObject)

foreign import ccall unsafe cf_boolean_true :: CFBooleanRef
foreign import ccall unsafe cf_boolean_false :: CFBooleanRef
foreign import ccall unsafe "Carbon/Carbon.h CFBooleanGetValue"
  cf_boolean_get_value :: CFBooleanRef -> IO C.CChar

-- toBool :: CFBooleanRef -> IO Bool
-- toBool = fmap Foreign.toBool . cf_boolean_get_value

-- fromBool :: Bool -> CFBooleanRef
-- fromBool = bool cf_boolean_false cf_boolean_true

instance IOIso Bool CFBooleanRef where
  ioTo = pure . bool cf_boolean_false cf_boolean_true
  ioFrom = fmap Foreign.toBool . cf_boolean_get_value

instance IOIso Bool Boolean where
  ioTo = bool booleanFalse booleanTrue
  ioFrom = flip withCFPtr ioFrom

booleanFalse :: IO Boolean
booleanFalse = Boolean <$> newForeignPtr_ cf_boolean_false

booleanTrue :: IO Boolean
booleanTrue = Boolean <$> newForeignPtr_ cf_boolean_true
