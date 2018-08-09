module AppleSdk.Framework.Carbon.EventTypeSpec where

#include <Carbon/Carbon.h>

import Foreign

data EventTypeSpec = EventTypeSpec
  { eventClass :: Word32
  , eventKind  :: Word32
  } deriving (Show)

instance Storable EventTypeSpec where
  sizeOf _ = #{size EventTypeSpec}
  alignment _ = #{alignment EventTypeSpec}
  peek p = do
    cl <- #{peek EventTypeSpec, eventClass} p
    ki  <- #{peek EventTypeSpec, eventKind} p
    return $ EventTypeSpec cl ki
  poke p (EventTypeSpec cl ki) = do
    #{poke EventTypeSpec, eventClass} p cl
    #{poke EventTypeSpec, eventKind} p ki
