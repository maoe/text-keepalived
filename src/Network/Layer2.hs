module Network.Layer2 where
import Data.Word
import Text.Printf

data MACAddr = MACAddr Word8 Word8 Word8 Word8 Word8 Word8 deriving Eq

instance Show MACAddr where
  show (MACAddr o1 o2 o3 o4 o5 o6) =
     printf "%02x:%02x:%02x:%02x:%02x:%02x" o1 o2 o3 o4 o5 o6