module AppleSdk.Framework.CoreGraphics.Connection where

import Data.Int

type ConnectionID = Int32

foreign import ccall unsafe "CGSMainConnectionID"
  mainConnection :: IO ConnectionID

foreign import ccall unsafe "_CGSDefaultConnection"
  defaultConnection :: IO ConnectionID

defaultConnection' :: ConnectionID
defaultConnection' = 217615
