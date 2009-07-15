{-# LANGUAGE PatternGuards #-}
module Main where
import Text.Keepalived.Types
import Text.Keepalived
import Network.Layer3
import Network.Layer4
import Control.Applicative
import Data.List
import Control.Monad.Identity

-- top level functions
vips' :: KeepalivedConf -> [CIDR]
vips' (GVrrpInstance vi) = virtualIpaddress vi ++ virtualIpaddressExcluded vi
vips' _                  = []

vips :: [KeepalivedConf] -> [CIDR]
vips = concatMap vips'

rips' :: KeepalivedConf -> [L4Addr]
rips' (GVirtualServer vs) = realServerAddress <$> realServers vs
rips' _                   = []

rips :: [KeepalivedConf] -> [L4Addr]
rips = concatMap rips'

-- low level functions
isActive :: RealServer -> Bool
isActive rs
  | weight rs == 0 = False
  | otherwise      = True

isStandby :: RealServer -> Bool
isStandby = not . isActive

-- testing

lvs01 = "/home/maoe/workspace/hatena/servers/keepalived/keepalived.lvs01.conf"
backend = "/home/maoe/workspace/hatena/servers/conf/keepalived/backend/master/keepalived.conf"
db = "/home/maoe/workspace/hatena/servers/conf/keepalived/db/backup/keepalived.conf"
lvs02 = "/home/maoe/workspace/hatena/servers/keepalived/keepalived.lvs02.conf"

testr file rip = do
  parsed <- parseFromFile file
  return $ lookupR (runIdentity $ ipAddr rip) parsed

lookupR :: IPAddr -> [KeepalivedConf] -> [KeepalivedConf]
lookupR a = concatMap go
  where go :: KeepalivedConf -> [KeepalivedConf]
        go gvs@(GVirtualServer vs)
          | rs     <- realServers vs
          , Just _ <- find isActiveAndMatch rs = [gvs]
          | otherwise                          = []
        go _                                   = []
        isActiveAndMatch :: RealServer -> Bool
        isActiveAndMatch rs
          | isActive rs, l4IpAddr (realServerAddress rs) == a = True
          | otherwise                                         = False
                                        
