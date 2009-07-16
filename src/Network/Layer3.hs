module Network.Layer3 where
import Data.Word
import Data.Bits
import Data.List
import Text.Parsec
import Control.Applicative
import Control.Monad

--
-- IP Address
--
newtype IPAddr = IPAddr { unIPAddr :: Word32 } deriving (Eq, Ord)
instance Show IPAddr where
  show = dots . toOctets . unIPAddr
instance Enum IPAddr where
  toEnum   = IPAddr . fromIntegral
  fromEnum = fromIntegral . unIPAddr

ipAddr :: Monad m => String -> m IPAddr
ipAddr ss = do x <- runParserT pIPAddr () "" ss
               case x of
                 Right xs -> return xs
                 Left err -> error $ show err

-- parsers
pOctet :: Monad m => ParsecT String u m Word32
pOctet = do
  octet <- read <$> many1 digit
  when (octet > 2^32-1) (error "illigal octet")
  return $ fromIntegral octet

pDot :: Monad m => ParsecT String u m Char
pDot = char '.'

pIPAddr :: Monad m => ParsecT String u m IPAddr
pIPAddr = do
  o1 <- pOctet; pDot
  o2 <- pOctet; pDot
  o3 <- pOctet; pDot
  o4 <- pOctet
  spaces
  return $ IPAddr $ foldl (\x y -> x `shiftL` 8 + y) 0 [o1, o2, o3, o4]

--
-- Netmask
--
newtype Netmask = Netmask { unNetmask :: Word8 }
instance Show Netmask where
  show = show . unNetmask

netmaskToOct :: Netmask -> String
netmaskToOct (Netmask w8) = dots $ toOctets w32
  where w32 = complement (2^(32-w8)) + 1

octToNetmask :: String -> Netmask
octToNetmask = Netmask . (32 -) . round . logBase 2 . fromIntegral . complement . fromOctets . unDots

{-
netmask :: String -> Netmask
netmask ss = case (runParserT pNetmask () "" ss) of
               Right xs -> xs
-}
-- parsers
pNetmask :: Monad m => ParsecT String u m Netmask
pNetmask = Netmask . read <$> many1 digit

pNetmaskOctets :: Monad m => ParsecT String u m Netmask
pNetmaskOctets = do
  octets <- many1 digit `sepBy1` pDot
  return $ Netmask $ f $ octets
  where f :: [String] -> Word8
        f = (32 -) . round . logBase 2 . fromIntegral . complement . fromOctets . map (read :: String -> Word8)

--
-- CIDR
--
data CIDR = CIDR { cIPAddr :: IPAddr, cNetmask :: Netmask }
instance Show CIDR where
  show (CIDR addr mask) = show addr ++ "/" ++ show mask

{-
cidr :: String -> CIDR
cidr ss = case (runParserT pCIDR () "" ss) of
            Right xs -> xs
-}

-- parsers
pCIDR :: Monad m => ParsecT String u m CIDR
pCIDR = do
  a <- pIPAddr
  m <- optionMaybe $ char '/' >> pNetmask
  return $ CIDR a (maybe (Netmask 32) id m)

-- common utils
dots :: [Word8] -> String
dots = concat . intersperse "." . map show

unDots :: String -> [Word8]
unDots = unfoldr go
  where go :: String -> Maybe (Word8, String)
        go s = case span (/= '.') s of
                 ("", _) -> Nothing
                 (o, "") -> Just (read o, "")
                 (o,  s) -> Just (read o, tail s)

toOctets :: Word32 -> [Word8]
toOctets w32 = map fromIntegral [o1, o2, o3, o4]
  where o1 = w32 `shiftR` 24
        o2 = w32 `shiftR` 16 .&. 0xff
        o3 = w32 `shiftR`  8 .&. 0xff
        o4 = w32             .&. 0xff


fromOctets :: [Word8] -> Word32
fromOctets [o1,o2,o3,o4] = fromIntegral o1 `shiftL` 24
                         + fromIntegral o2 `shiftL` 16
                         + fromIntegral o3 `shiftL` 8
                         + fromIntegral o4
fromOctets _             = error "illigal arguments"
