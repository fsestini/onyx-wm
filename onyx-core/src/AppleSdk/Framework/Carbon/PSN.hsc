module AppleSdk.Framework.Carbon.PSN where

#include <Carbon/Carbon.h>

import Foreign

data PSN = PSN
  { psnHigh :: Word32
  , psnLow :: Word32
  } deriving (Show)

nullPSN :: PSN
nullPSN = PSN 0 0

instance Storable PSN where
  sizeOf _ = #{size ProcessSerialNumber}
  alignment _ = #{alignment ProcessSerialNumber}
  peek p = do
    high <- #{peek ProcessSerialNumber, highLongOfPSN} p
    low  <- #{peek ProcessSerialNumber, lowLongOfPSN} p
    return $ PSN high low
  poke p (PSN high low) = do
    #{poke ProcessSerialNumber, highLongOfPSN} p high
    #{poke ProcessSerialNumber, lowLongOfPSN} p low
