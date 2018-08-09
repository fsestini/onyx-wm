{-# LANGUAGE TypeFamilies #-}

module AppleSdk.Framework.Accessibility.Value where

import AppleSdk.Framework.CoreFoundation.Object
import Foreign.Ptr
import Foreign

data AXValue
type AXValueRef = Ptr AXValue
newtype Value = Value { getValue :: ForeignPtr AXValue }

instance CoreFoundationType AXValue
instance CFObject Value where
  type CFT Value = AXValue
  toCFPtr = getValue
  fromCFPtr = Value
