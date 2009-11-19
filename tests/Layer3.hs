import Network.Layer3
import Test.QuickCheck

instance Arbitrary Word8 where
  arbitrary = fromIntegral <$> choose (0, 2^8-1 :: Int)

instance Arbitrary Word32 where
  arbitrary = fromIntegral <$> choose (0, 2^32-1 :: Integer)

newtype IPAddrStr = IPAddrStr { unIPAddrStr :: String } deriving Show

instance Arbitrary IPAddrStr where
  arbitrary = do
    o1 <- choose (0, 2^8-1 :: Int)
    o2 <- choose (0, 2^8-1) 
    o3 <- choose (0, 2^8-1)
    o4 <- choose (0, 2^8-1) 
    return $ IPAddrStr $ concat $ intersperse "." $ map show [o1, o2, o3, o4]

newtype CIDRStr = CIDRStr { unCIDRStr :: String } deriving Show

instance Arbitrary CIDRStr where
  arbitrary = do
    i <- arbitrary :: Gen IPAddrStr
    n <- arbitrary :: Gen Word8
    return $ CIDRStr $ unIPAddrStr i ++ "/" ++ show n

prop_IPAddr :: Word32 -> Bool
prop_IPAddr w = w == unIPAddr (IPAddr w)

{-
prop_ipAddr :: IPAddrStr -> Bool
prop_ipAddr ss = unIPAddrStr ss == show (ipAddr (unIPAddrStr ss))
-}

prop_Netmask :: Word8 -> Bool
prop_Netmask w = w == unNetmask (Netmask w)

{-
prop_netmask :: Word8 -> Bool
prop_netmask w = w == unNetmask (netmask (show w))
-}

prop_CIDR :: Word32 -> Word8 -> Bool
prop_CIDR w32 w8 = w32 == unIPAddr (cIPAddr c) && w8  == unNetmask (cNetmask c)
   where c = CIDR (IPAddr w32) (Netmask w8)

{-
prop_cidr :: CIDRStr -> Bool
prop_cidr ss = unCIDRStr ss == show (cidr (unCIDRStr ss))
-}
