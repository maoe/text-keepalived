module Network.Layer4 where
import Data.Word
import Network.Layer3

data L4Addr = L4Addr { l4IpAddr :: IPAddr
                     , l4Port   :: PortNumber
                     } deriving Eq

instance Ord L4Addr where
  (L4Addr addr port) `compare` (L4Addr addr' port') =
    case  addr `compare` addr' of
      EQ -> port `compare` port'
      x  -> x

newtype PortNumber = PortNumber { unPortNumber :: Word16 }
                     deriving (Eq, Ord)

instance Show PortNumber where
  show = show . unPortNumber

instance Show L4Addr where
  show (L4Addr i p) = show i ++ ":" ++ show p