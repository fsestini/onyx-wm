{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module AppleSdk.Framework.CoreFoundation.Number
  ( CFNumberRef
  , Number
  , NumberType(..)
  , numberCreate
  , numberCreate'
  , numberGetValue
  ) where

import Control.Monad.Managed
import Foreign hiding (with)
import Foreign.C.Types (CInt(..), CBool(..))
import AppleSdk.Framework.CoreFoundation.Allocator
import AppleSdk.Framework.CoreFoundation.Object

data CFNumber
type CFNumberRef = Ptr CFNumber
instance CoreFoundationType CFNumber
newtype Number = Number (ForeignPtr CFNumber) deriving CFObject

type CFNumberType = CInt

data NumberTypeTy = NumberSigned64TypeTy | NumberLongTypeTy | NumberIntTypeTy

data NumberType (ty :: NumberTypeTy) where
  NumberSigned64Type :: NumberType NumberSigned64TypeTy
  NumberLongType :: NumberType NumberLongTypeTy
  NumberIntType :: NumberType NumberIntTypeTy

type family NumTy (nty :: NumberTypeTy) :: * where
  NumTy NumberSigned64TypeTy = Int64
  NumTy NumberLongTypeTy = Int64
  NumTy NumberIntTypeTy = Int32

storableTy :: NumberType ty -> (Storable (NumTy ty) => b) -> b
storableTy NumberSigned64Type f = f
storableTy NumberLongType f = f
storableTy NumberIntType f = f

toCFNumberType :: NumberType ty -> CFNumberType
toCFNumberType NumberSigned64Type = 4
toCFNumberType NumberIntType = 9
toCFNumberType NumberLongType = 10

--------------------------------------------------------------------------------

foreign import ccall unsafe "CFNumberCreate"
  cfNumberCreate :: CFAllocatorRef -> CFNumberType -> Ptr a -> IO CFNumberRef

numberCreate :: Allocator -> NumberType ty -> NumTy ty -> IO Number
numberCreate allo nty num = flip with pure $ do
  allo' <- managed (withCFPtr allo)
  storableTy nty $ do
    p <- managed alloca
    liftIO $ do
      poke p num
      num' <- cfNumberCreate allo' (toCFNumberType nty) p
      manageCFObj num'

numberCreate' :: NumberType ty -> NumTy ty -> IO Number
numberCreate' nty num = nullAllocator >>= \a -> numberCreate a nty num

--------------------------------------------------------------------------------

foreign import ccall unsafe "CFNumberGetValue"
  cfNumberGetValue :: CFNumberRef -> CFNumberType -> Ptr a -> IO CBool

numberGetValue :: Number -> NumberType ty -> IO (Maybe (NumTy ty))
numberGetValue num nty = flip with pure $ do
  nptr <- managed (withCFPtr num)
  storableTy nty $ do
    p <- managed alloca
    liftIO $ do
      b <- cfNumberGetValue nptr (toCFNumberType nty) p
      if toBool b then Just <$> peek p else pure Nothing
