{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Text.Keepalived.Types
  ( -- * Top-level types
    KeepalivedConf (..)
  , KeepalivedConfType (..)
    -- * Internal types
    -- ** Global Definitions
  , GlobalDefs (..)
    -- ** Static routes / IP addresses
  , Route (..)
  , Ipaddress (..)
    -- ** VRRP scrpts
  , VrrpScript (..)
    -- ** VRRP sync groups
  , VrrpSyncGroup (..)
    -- ** VRRP instances
  , VrrpInstance (..)
  , TrackInterface (..)
  , TrackScript (..)
  , Auth (..)
  , AuthType (..)
  , Vrid (..)
  , Priority (..)
  , VrrpState (..)
  , VrrpNotify (..)
    -- ** Virtual server groups
  , VirtualServerGroup (..)
  , VirtualServerGroupMember (..)
    -- ** Virtual servers
  , VirtualServer (..)
  , VirtualServerId (..)
  , LvsMethod (..)
  , LvsSched (..)
  , Protocol (..)
  , RealServer (..)
  , RealServerAddress (..)
  , RealServerNotify (..)
  , HttpGet (..)
  , Url (..)
  , SmtpCheck (..)
  , SmtpCheckOption (..)
  , Host (..)
  , TcpCheck (..)
  , MiscCheck (..)
  ) where

import Network.Layer3
import Network.Layer4
import Data.Function
import Data.Generics
import Data.Word
import Text.PrettyPrint.HughesPJ


-- | Top-level type for keepalived.conf
data KeepalivedConf = KeepalivedConf [KeepalivedConfType] deriving (Typeable, Data)

-- | Each top-level directives
data KeepalivedConfType = TGlobalDefs         GlobalDefs         -- ^ global_defs directive
                        | TStaticRoutes       [Route]            -- ^ static_routes directive
                        | TStaticIpaddress    [Ipaddress]        -- ^ static_ipaddress directive
                        | TVrrpScript         VrrpScript         -- ^ vrrp_script directive
                        | TVrrpSyncGroup      VrrpSyncGroup      -- ^ vrrp_sync_group directives
                        | TVrrpInstance       VrrpInstance       -- ^ vrrp_instance directives
                        | TVirtualServerGroup VirtualServerGroup -- ^ virtual_server_group directive
                        | TVirtualServer      VirtualServer      -- ^ virtual_server directive
                        deriving (Eq, Ord, Typeable, Data)
                  
-- GLOBAL CONFIGURATION
-- | global definitions
data GlobalDefs = GlobalDefs
  { notificationEmail     :: [String]      -- ^ notification_email direcitive
  , notificationEmailFrom :: Maybe String  -- ^ notification_email_from directive
  , smtpServer            :: Maybe IPAddr  -- ^ smtp_server directive
  , smtpConnectTimeout    :: Maybe Integer -- ^ smtp_connect_timeout directive
  , routerId              :: Maybe String  -- ^ router_id directive
  } deriving (Eq, Ord, Typeable, Data)

-- | Routing formats based on @ip route@ command
data Route =
   -- | Black hole filtering
   Blackhole CIDR
   -- | Normal routing
 | Route { src      :: Maybe IPAddr  -- ^ Source address
         , dest     :: CIDR          -- ^ Destination address
         , via      :: Maybe IPAddr  -- ^ Gateway address
         , or       :: Maybe IPAddr  -- ^ Additional gateway address
         , rDev     :: Maybe String  -- ^ Device name
         , rScope   :: Maybe String  -- ^ Scope??
         , table    :: Maybe String  -- ^ Routing table
         , metric   :: Maybe Integer -- ^ Metric
         }
  deriving (Eq, Ord, Typeable, Data)

-- | IP address formats based on @ip@ command
data Ipaddress = Ipaddress
  { iDest  :: CIDR         -- ^ Destination address
  , brd    :: Maybe IPAddr -- ^ Broadcast address
  , iDev   :: Maybe String -- ^ Device name
  , iScope :: Maybe String -- ^ Scope??
  , label  :: Maybe String -- ^ Label??
  } deriving (Eq, Typeable, Data)

instance Ord Ipaddress where
  compare = compare `on` iDest

-- VRRPD CONFIGURATION
-- | vrrp script
data VrrpScript = VrrpScript
  { scriptId              :: String         -- ^ vrrp_script identifier
  , vrrpScript            :: String         -- ^ Script to run periodically
  , scriptInterval        :: Integer        -- ^ Run the script this every seconds
  , scriptWeight          :: Maybe Integer  -- ^ Adjust priority by this weight
  } deriving (Eq, Ord, Typeable, Data)

-- | vrrp_sync_group
data VrrpSyncGroup = VrrpSyncGroup
  { syncName              :: String       -- ^ vrrp_sync_group identifier
  , syncGroup             :: [String]     -- ^ A set of vrrp_instance identifiers to sync together
  , syncNotify            :: [VrrpNotify] -- ^ notify, notify_master, notify_backup or notify_fault
  , syncSmtpAlert         :: Maybe ()     -- ^ Send email notif during state transit
  } deriving (Eq, Ord, Typeable, Data)

-- | vrrp_instance
data VrrpInstance = VrrpInstance
  { vrrpName              :: String           -- ^ vrrp_instance identifier
  , vrrpInterface         :: String           -- ^ Binding interface
  , vrid                  :: Vrid             -- ^ VRRP VRID
  , priority              :: Priority         -- ^ VRRP PRIO
  , virtualIpaddress      :: [Ipaddress]      -- ^ Virtual IP addresses
  , virtualIpaddressExcluded :: [Ipaddress]   -- ^ VRRP IP excluded from VRRP
  , trackInterfaces       :: [TrackInterface] -- ^ Interfaces state we monitor
  , trackScript           :: [TrackScript]    -- ^ Script state we monitor
  , virtualRoutes         :: [Route]          -- ^ VRP virtual routes
  , vrrpNotify            :: [VrrpNotify]     -- ^ notify_master, notify_backup, notify_fault, notify_stop or notify directive
  , vrrpState             :: Maybe VrrpState  -- ^ Start-up default state
  , smtpAlert             :: Maybe ()         -- ^ Send email notif during state transit
  , dontTrackPrimary      :: Maybe ()         -- ^ Ignore VRRP interface faults
  , mcastSrcIp            :: Maybe IPAddr     -- ^ Sources IP address to use into the VRRP packets
  , lvsSyncDaemon         :: Maybe String     -- ^ Binding interface for LVS syncd
  , garpMasterDelay       :: Maybe Integer    -- ^ Delay for gratuitous ARP after MASTER state transition
  , advertInt             :: Maybe Integer    -- ^ VRRP Advert interval
  , authentication        :: Maybe Auth       -- ^ VRRP authentication
  , noPreempt             :: Maybe ()         -- ^ Override VRRP RFC preemption default
  , preemptDelay          :: Maybe Integer    -- ^ Seconds after startup until preemption
  , debug                 :: Maybe ()         -- ^ Debug level
  } deriving (Eq, Ord, Typeable, Data)

-- | @track_interface@
data TrackInterface = TrackInterface
  { trackInterface       :: String        -- ^ An interface state we monitor
  , trackInterfaceWeight :: Maybe Integer -- ^ -254 .. 254
  } deriving (Eq, Ord, Typeable, Data)

-- | @script@ in @track_interface@
data TrackScript = TrackScript
  { trackScriptName   :: String           -- ^ Scripts state we monitor
  , trackScriptWeight :: Maybe Integer    -- ^ -254 .. 254
  } deriving (Eq, Ord, Typeable, Data)

-- | @state@ in @vrrp_instance@
data VrrpState = VrrpMaster
               | VrrpBackup
               deriving (Eq, Ord, Typeable, Data)

-- | @auth@ in @vrrp_instance@
data Auth = Auth
  { authType              :: AuthType     -- ^ Authentication type (PASS|AH)
  , authPass              :: String       -- ^ Password for accessing vrrpd
  } deriving (Eq, Ord, Typeable, Data)

data AuthType = PASS  -- ^ Simple passwd (suggested)
              | AH    -- ^ IPSec (not recommended)
              deriving (Eq, Ord, Typeable, Data)

newtype Vrid = Vrid { unVrid :: Word8 } deriving (Eq, Ord, Typeable, Data)
instance Show Vrid where
  show = show . unVrid

newtype Priority = Priority { unPriority :: Word8 } deriving (Eq, Ord, Typeable, Data)
instance Show Priority where
  show = show . unPriority

-- LVS CONFIGURATION
-- virtual server group(s)
data VirtualServerGroup = VirtualServerGroup
  { groupName              :: String
  , groupMembers           :: [VirtualServerGroupMember]
  } deriving (Eq, Ord, Typeable, Data)

data VirtualServerGroupMember = VirtualServerIPAddress IPAddr Integer
                              | VirtualServerIPRange   IPAddr Integer Integer
                              | VirtualServerFwmark    Integer
                              deriving (Eq, Ord, Typeable, Data)

-- virtual server(s)
data VirtualServer = VirtualServer
  { virtualServerId        :: VirtualServerId
  , lvsMethod                 :: LvsMethod
  , lvsSched               :: LvsSched
  , protocol               :: Protocol
  , realServers            :: [RealServer]
  , sorryServer            :: Maybe L4Addr
  , delayLoop              :: Maybe Integer
  , virtualHost            :: Maybe String
  , persistenceTimeout     :: Maybe Integer
  , persistenceGranularity :: Maybe String
  , haSuspend              :: Maybe ()
  , alpha                  :: Maybe ()
  , omega                  :: Maybe ()
  , quorum                 :: Maybe Integer
  , hysteresis             :: Maybe Integer
  , quorumUp               :: Maybe String
  , quorumDown             :: Maybe String
  , natMask                :: Maybe Netmask
  } deriving (Eq, Ord, Typeable, Data)

data VirtualServerId = VirtualServerIpId     RealServerAddress
                     | VirtualServerFwmarkId Integer
                     | VirtualServerGroupId  String
                     deriving (Eq, Ord, Typeable, Data)

-- | LVS forwarding method
data LvsMethod = NAT -- ^ Virtual server via NAT
               | DR  -- ^ Virtual server via direct routing
               | TUN -- ^ Virtual server via IP tunneling
               deriving (Eq, Ord, Typeable, Data)

-- | LVS scheduler used
data LvsSched = RR   -- ^ Round-robin scheduling
              | WRR  -- ^ Weighted round-robin scheduling
              | LC   -- ^ Least-connection scheduling
              | WLC  -- ^ Weighted least-connection scheduling
              | LBLC -- ^ Locality-based least-connection scheduling
              | SH   -- ^ Source hashing scheduling
              | DH   -- ^ Destination hashing scheduling
              deriving (Eq, Ord, Typeable, Data)

data Protocol = TCP | UDP deriving (Eq, Ord, Typeable, Data)

data RealServer = RealServer
  { realServerAddress     :: RealServerAddress
  , weight                :: Integer
  , inhibitOnFailure      :: Maybe ()
  , httpGet               :: Maybe HttpGet
  , tcpCheck              :: Maybe TcpCheck
  , smtpCheck             :: Maybe SmtpCheck
  , miscCheck             :: Maybe MiscCheck
  , realServerNotify      :: [RealServerNotify]
  } deriving (Eq, Ord, Typeable, Data)

data RealServerAddress = RealServerAddress IPAddr (Maybe PortNumber)
                       deriving (Eq, Ord, Typeable, Data)

data HttpGet =
  HttpGet { httpUrl               :: [Url]
          , httpPort              :: Maybe Integer
          , httpBindto            :: Maybe IPAddr
          , httpTimeout           :: Maybe Integer
          , httpNbGetRetry        :: Maybe Integer
          , httpDelayBeforeRetry  :: Maybe Integer
          } |
  SslGet  { sslUrl                :: [Url]
          , sslPort               :: Maybe Integer
          , sslBindto             :: Maybe IPAddr
          , sslTimeout            :: Maybe Integer
          , sslNbGetRetry         :: Maybe Integer
          , sslDelayBeforeRetry   :: Maybe Integer
          }
  deriving (Eq, Ord, Typeable, Data)

data Url = Url
  { urlPath               :: String
  , urlDigest             :: Maybe String
  , urlStatusCode         :: Maybe Integer
  } deriving (Eq, Ord, Typeable, Data)

data TcpCheck = TcpCheck
  { tcpTimeout            :: Integer
  , tcpPort               :: Maybe Integer
  , tcpBindto             :: Maybe IPAddr
  } deriving (Eq, Ord, Typeable, Data)

data SmtpCheck = SmtpCheck [SmtpCheckOption] deriving (Eq, Ord, Typeable, Data)

data SmtpCheckOption = SmtpConnectTimeout    Integer
                     | SmtpHost              Host
                     | SmtpRetry             Integer
                     | SmtpDelayBeforeRetry  Integer
                     | SmtpHeloName          String
                     deriving (Eq, Ord, Typeable, Data)

data Host = Host
  { hostIP                :: Maybe IPAddr
  , hostPort              :: Maybe Integer
  , hostBindto            :: Maybe IPAddr
  } deriving (Eq, Ord, Typeable, Data)

data MiscCheck = MiscCheck
  { miscPath              :: String
  , miscTimeout           :: Maybe Integer
  , miscDynamic           :: Maybe ()
  } deriving (Eq, Ord, Typeable, Data)

data VrrpNotify = NotifyMaster String
                | NotifyBackup String
                | NotifyFault  String
                | Notify       String
                deriving (Eq, Ord, Typeable, Data)

data RealServerNotify = NotifyUp   String
                      | NotifyDown String
                      deriving (Eq, Ord, Typeable, Data)


-- Typeable and Data stand-alone deriving declarations
deriving instance Typeable CIDR
deriving instance Data     CIDR

deriving instance Typeable L4Addr
deriving instance Data     L4Addr

deriving instance Typeable Netmask
deriving instance Data     Netmask
deriving instance Ord      Netmask

deriving instance Typeable PortNumber
deriving instance Data     PortNumber

deriving instance Typeable IPAddr
deriving instance Data     IPAddr


-- Show instnance declarations
instance Show KeepalivedConf where
  show = render . renderKeepalivedConf

instance Show KeepalivedConfType where
  show = render . renderKeepalivedConfType

instance Show GlobalDefs where
  show = render . renderGlobalDefs

instance Show Route where
  show = render . renderRoute

instance Show Ipaddress where
  show = render . renderIpaddress

instance Show VrrpScript where
  show = render . renderVrrpScript

instance Show VrrpSyncGroup where
  show = render . renderVrrpSyncGroup

instance Show VrrpInstance where
  show = render . renderVrrpInstance

instance Show VrrpState where
  show = render . renderVrrpState

instance Show Auth where
  show = render . renderAuth

instance Show AuthType where
  show = render . renderAuthType

instance Show VirtualServerGroup where
  show = render . renderVirtualServerGroup

instance Show VirtualServerGroupMember where
  show = render . renderVirtualServerGroupMember

instance Show VirtualServer where
  show = render . renderVirtualServer

instance Show VirtualServerId where
  show = render . renderVirtualServerId

instance Show LvsMethod where
  show = render . renderLvsMethod

instance Show LvsSched where
  show = render . renderLvsSched

instance Show Protocol where
  show = render . renderProtocol

instance Show RealServer where
  show = render . renderRealServer

instance Show HttpGet where
  show = render . renderHttpGet

instance Show Url where
  show = render . renderUrl

instance Show TcpCheck where
  show = render . renderTcpCheck

instance Show SmtpCheck where
  show = render . renderSmtpCheck

instance Show Host where
  show = render . renderHost

instance Show MiscCheck where
  show = render . renderMiscCheck

instance Show VrrpNotify where
  show = render . renderVrrpNotify

instance Show RealServerNotify where
  show = render . renderRealServerNotify

-- pretty printer
renderKeepalivedConf :: KeepalivedConf -> Doc
renderKeepalivedConf (KeepalivedConf ts) = vcat $ map renderKeepalivedConfType ts

renderKeepalivedConfType :: KeepalivedConfType -> Doc
renderKeepalivedConfType (TGlobalDefs         d) = renderGlobalDefs d
renderKeepalivedConfType (TStaticRoutes       r) = renderStaticRoutes r
renderKeepalivedConfType (TStaticIpaddress    i) = renderStaticIpaddress i
renderKeepalivedConfType (TVrrpScript         s) = renderVrrpScript s
renderKeepalivedConfType (TVrrpSyncGroup      g) = renderVrrpSyncGroup g
renderKeepalivedConfType (TVrrpInstance       i) = renderVrrpInstance i
renderKeepalivedConfType (TVirtualServerGroup g) = renderVirtualServerGroup g
renderKeepalivedConfType (TVirtualServer      s) = renderVirtualServer s

renderGlobalDefs :: GlobalDefs -> Doc
renderGlobalDefs (GlobalDefs mail mailFrom smtp timeout routerId) =
  vcat [ text "global_defs" <+> lbrace
       , indent $ vcat [ renderNotificationEmail mail
                       , renderMaybe ((text "notification_email_from" <+>) . text) mailFrom
                       , renderMaybe ((text "smtp_server" <+>) . text . show) smtp
                       , renderMaybe ((text "smtp_connect_timeout" <+>) . integer) timeout
                       , renderMaybe ((text "router_id" <+>) . text) routerId ]
       , rbrace ]

renderStaticRoutes :: [Route] -> Doc
renderStaticRoutes [] = empty
renderStaticRoutes r  =
  vcat [ text "static_routes" <+> lbrace
       , indent $ vcat $ map renderRoute r
       , rbrace ]

renderStaticIpaddress :: [Ipaddress] -> Doc
renderStaticIpaddress [] = empty
renderStaticIpaddress i  =
  vcat [ text "static_ipaddress" <+> lbrace
       , indent $ vcat $ map renderIpaddress i
       , rbrace ]

renderRoute :: Route -> Doc
renderRoute (Blackhole c) = text "blackhole" <+> text (show c)
renderRoute (Route src dest via or dev scp tbl mtr) =
   hsep [ renderMaybe ((text "src" <+>) . text . show) src
        , text (show dest)
        , renderMaybe ((text "via" <+>) . text . show) via
        , renderMaybe ((text "or" <+>) . text . show) or
        , renderMaybe ((text "dev" <+>) . text) dev
        , renderMaybe ((text "scope" <+>) . text) scp
        , renderMaybe ((text "table" <+>) . text) tbl
        , renderMaybe ((text "metric" <+>) . integer) mtr ]

renderNotificationEmail :: [String] -> Doc
renderNotificationEmail [] = empty
renderNotificationEmail ms =
  vcat [ text "notification_email" <+> lbrace
       , indent $ vcat $ map text ms
       , rbrace ]

renderVrrpScript :: VrrpScript -> Doc
renderVrrpScript (VrrpScript ident script interval weight) =
  vcat [ text "vrrp_script" <+> text ident <+> lbrace
       , indent $ vcat [ text "script" <+> doubleQuotes (text script)
                       , text "interval" <+> integer interval
                       , renderMaybe ((text "weight" <+>) . integer) weight ]
       , rbrace ]

renderVrrpSyncGroup :: VrrpSyncGroup -> Doc
renderVrrpSyncGroup (VrrpSyncGroup name group notify alert) =
  vcat [ text "vrrp_sync_group" <+> text name <+> lbrace
       , indent $ vcat [ renderGroup group
                       , vcat (map renderVrrpNotify notify)
                       , renderMaybe (const (text "smtp_alert")) alert ]
       , rbrace ]

renderGroup :: [String] -> Doc
renderGroup g =
  vcat [ text "group" <+> lbrace
       , indent $ vcat (map text g)
       , rbrace ]

renderVrrpInstance :: VrrpInstance -> Doc
renderVrrpInstance vi =
  vcat [ text "vrrp_instance" <+> text (vrrpName vi) <+> lbrace
       , indent $ vcat [ text "interface" <+> text (vrrpInterface vi)
                       , text "priority" <+> text (show (priority vi))
                       , renderMaybe renderVrrpState (vrrpState vi)
                       , text "virtual_router_id" <+> text (show (vrid vi))
                       , renderTrackInterface (trackInterfaces vi)
                       , renderTrackScript (trackScript vi)
                       , renderVirtualIpaddress (virtualIpaddress vi)
                       , renderVirtualIpaddressExcluded (virtualIpaddressExcluded vi)
                       , renderVirtualRoutes (virtualRoutes vi)
                       , vcat $ map renderVrrpNotify (vrrpNotify vi)
                       , renderMaybe (const $ text "smtp_alert") (smtpAlert vi)
                       , renderMaybe (const $ text "dont_track_primary") (dontTrackPrimary vi)
                       , renderMaybe ((text "mcast_src_ip" <+>) . text . show) (mcastSrcIp vi)
                       , renderMaybe ((text "lvs_sync_daemon_interface" <+>) . text) (lvsSyncDaemon vi)
                       , renderMaybe ((text "garp_master_delay" <+>) . integer) (garpMasterDelay vi)
                       , renderMaybe ((text "advert_int" <+>) . integer) (advertInt vi)
                       , renderMaybe renderAuth (authentication vi)
                       , renderMaybe (const $ text "nopreempt") (noPreempt vi)
                       , renderMaybe ((text "preempt_delay" <+>) . integer) (preemptDelay vi)
                       , renderMaybe (const $ text "debug") (debug vi)
                       ]
       , rbrace ]

renderIpaddress :: Ipaddress -> Doc
renderIpaddress (Ipaddress dst brd dev scp lbl) =
  hsep [ text (show dst)
       , renderMaybe ((text "brd" <+>) . text . show) brd
       , renderMaybe ((text "dev" <+>) . text) dev
       , renderMaybe ((text "scope" <+>) . text) scp
       , renderMaybe ((text "label" <+>) . text) lbl ]

renderVirtualIpaddress :: [Ipaddress] -> Doc
renderVirtualIpaddress [] = empty
renderVirtualIpaddress xs =
  vcat [ text "virtual_ipaddress" <+> lbrace
       , indent $ vcat $ map renderIpaddress xs
       , rbrace ]

renderVirtualIpaddressExcluded :: [Ipaddress] -> Doc
renderVirtualIpaddressExcluded [] = empty
renderVirtualIpaddressExcluded xs =
  vcat [ text "virtual_ipaddress_excluded" <+> lbrace
       , indent $ vcat $ map renderIpaddress xs
       , rbrace ]

renderTrackInterface :: [TrackInterface] -> Doc
renderTrackInterface [] = empty
renderTrackInterface t  =
  vcat [ text "track_interface" <+> lbrace
       , indent $ vcat $ map renderTrackInterfaceLine t
       , rbrace ]

renderTrackInterfaceLine :: TrackInterface -> Doc
renderTrackInterfaceLine (TrackInterface n w) = text n <+> renderMaybe ((text "weight" <+>) . integer) w

renderTrackScript :: [TrackScript] -> Doc
renderTrackScript [] = empty
renderTrackScript ts =
  vcat [ text "track_script" <+> lbrace
       , indent $ vcat $ map renderTrackScriptLine ts
       , rbrace ]

renderTrackScriptLine :: TrackScript -> Doc
renderTrackScriptLine (TrackScript n w) = text n <+> renderMaybe ((text "weight" <+>) . integer) w

renderVirtualRoutes :: [Route] -> Doc
renderVirtualRoutes [] = empty
renderVirtualRoutes r  =
  vcat [ text "virtual_routes" <+> lbrace
       , indent $ vcat (map renderRoute r)
       , rbrace ]

renderVrrpState :: VrrpState -> Doc
renderVrrpState s = text "state" <+> state s
  where state VrrpMaster = text "MASTER"
        state VrrpBackup = text "BACKUP"

renderAuth :: Auth -> Doc
renderAuth (Auth typ pass) =
  vcat [ text "auth" <+> lbrace
       , indent $ vcat [ text "auth_type" <+> renderAuthType typ
                       , text "auth_pass" <+> text pass ]
       , rbrace ]

renderAuthType :: AuthType -> Doc
renderAuthType t = text "auth_type" <+> typ t
  where typ PASS = text "PASS"
        typ AH   = text "AH"

renderVirtualServerGroup :: VirtualServerGroup -> Doc
renderVirtualServerGroup (VirtualServerGroup name members) =
  vcat [ text "virtual_server_group" <+> text name <+> lbrace
       , indent $ vcat $ map renderVirtualServerGroupMember members
       , rbrace ]

renderVirtualServerGroupMember :: VirtualServerGroupMember -> Doc
renderVirtualServerGroupMember (VirtualServerIPAddress i p) = text (show i) <+> integer p
renderVirtualServerGroupMember (VirtualServerIPRange i d p) = text (show i) <> text "-" <> integer d <+> integer p
renderVirtualServerGroupMember (VirtualServerFwmark i)      = text "fwmark" <+> integer i

renderVirtualServer :: VirtualServer -> Doc
renderVirtualServer vs =
  vcat [ text "virtual_server" <+> renderVirtualServerId (virtualServerId vs) <+> lbrace
       , indent $ vcat [ renderLvsMethod (lvsMethod vs)
                       , renderLvsSched (lvsSched vs)
                       , renderProtocol (protocol vs)
                       , vcat $ map renderRealServer (realServers vs)
                       , renderMaybe ((text "sorry_server" <+>) . renderCIDR) (sorryServer vs)
                       , renderMaybe ((text "delay_loop" <+>) . integer) (delayLoop vs)
                       , renderMaybe ((text "virtualhost" <+>) . text) (virtualHost vs)
                       , renderMaybe ((text "persistence_timeout" <+>) . integer) (persistenceTimeout vs)
                       , renderMaybe ((text "persistence_granularity" <+>) . text) (persistenceGranularity vs)
                       , renderMaybe (const (text "ha_suspend")) (haSuspend vs)
                       , renderMaybe (const (text "alpha")) (alpha vs)
                       , renderMaybe (const (text "omega")) (omega vs)
                       , renderMaybe ((text "quorum" <+>) . integer) (quorum vs)
                       , renderMaybe ((text "hysteresis" <+>) . integer) (hysteresis vs)
                       , renderMaybe ((text "quorum_up" <+>) . text) (quorumUp vs)
                       , renderMaybe ((text "quorum_down" <+>) . text) (quorumDown vs)
                       , renderMaybe ((text "nat_mask" <+>) . text . show) (natMask vs) ]
       , rbrace ]
  where renderCIDR cidr = text (show (l4IpAddr cidr)) <+> text (show (l4Port cidr))

renderVirtualServerId :: VirtualServerId -> Doc
renderVirtualServerId (VirtualServerIpId (RealServerAddress addr port)) = text (show addr) <+> renderMaybe (text . show) port
renderVirtualServerId (VirtualServerFwmarkId i)    = text "fwmark" <+> integer i
renderVirtualServerId (VirtualServerGroupId ident) = text "group"  <+> text ident

renderLvsMethod :: LvsMethod -> Doc
renderLvsMethod k = text "lvs_method" <+> kind k
  where kind NAT = text "NAT"
        kind DR  = text "DR"
        kind TUN = text "TUN"

renderLvsSched :: LvsSched -> Doc
renderLvsSched a = text "lvs_sched" <+> algo a
  where algo RR   = text "rr"
        algo WRR  = text "wrr"
        algo LC   = text "lc"
        algo WLC  = text "wlc"
        algo LBLC = text "lblc"
        algo SH   = text "sh"
        algo DH   = text "dh"

renderProtocol :: Protocol -> Doc
renderProtocol p = text "protocol" <+> proto p
  where proto TCP = text "TCP"
        proto UDP = text "UDP"

renderRealServer :: RealServer -> Doc
renderRealServer (RealServer (RealServerAddress addr port) weight inhibit httpGet tcpCheck smtpCheck miscCheck notify) =
  vcat [ text "real_server" <+> addr' <+> lbrace
       , indent $ vcat [ weight'
                       , inhibit'
                       , httpGet'
                       , tcpCheck'
                       , smtpCheck'
                       , miscCheck'
                       , notify' ]
       , rbrace ]
  where addr'      = text (show addr) <+> renderMaybe (text . show) port
        weight'    = text "weight" <+> integer weight
        inhibit'   = renderMaybe (const $ text "inhibit_on_failure") inhibit
        httpGet'   = renderMaybe renderHttpGet httpGet
        tcpCheck'  = renderMaybe renderTcpCheck tcpCheck
        smtpCheck' = renderMaybe renderSmtpCheck smtpCheck
        miscCheck' = renderMaybe renderMiscCheck miscCheck
        notify'    = vcat $ map renderRealServerNotify notify

renderHttpGet :: HttpGet -> Doc
renderHttpGet (HttpGet url port bindto timeout nb delay) =
  vcat [ text "HTTP_GET" <+> lbrace
       , indent $ vcat [ vcat (map renderUrl url)
                       , renderMaybe renderConnectPort port
                       , renderMaybe renderBindto bindto
                       , renderMaybe renderConnectTimeout timeout
                       , renderMaybe ((text "nb_get_retry" <+>) . integer) nb
                       , renderMaybe ((text "delay_before_retry" <+>) . integer) delay ]
       , rbrace ]
renderHttpGet (SslGet  url port bindto timeout nb delay) =
  vcat [ text "SSL_GET" <+> lbrace
       , indent $ vcat [ vcat (map renderUrl url)
                       , renderMaybe renderConnectPort port
                       , renderMaybe renderBindto bindto
                       , renderMaybe renderConnectTimeout timeout
                       , renderMaybe ((text "nb_get_retry" <+>) . integer) nb
                       , renderMaybe ((text "delay_before_retry" <+>) . integer) delay ]
       , rbrace ]

renderUrl :: Url -> Doc
renderUrl (Url path digest status) =
  vcat [ text "url" <+> lbrace
       , indent $ vcat [ path'
                       , digest'
                       , status' ]
       , rbrace ]
  where path'   = text "path" <+> text path
        digest' = renderMaybe ((text "digest" <+>) . text) digest
        status' = renderMaybe ((text "status_code" <+>) . integer) status

renderTcpCheck :: TcpCheck -> Doc
renderTcpCheck (TcpCheck timeout port bindto) =
  vcat [ text "TCP_CHECK" <+> lbrace
       , indent $ vcat [ timeout'
                       , port'
                       , bindto' ]
       , rbrace ]
  where timeout' = renderConnectTimeout timeout
        port'    = renderMaybe renderConnectPort port
        bindto'  = renderMaybe renderBindto bindto

renderSmtpCheck :: SmtpCheck -> Doc
renderSmtpCheck (SmtpCheck opts) =
  vcat [ text "SMTP_CHECK" <+> lbrace
       , indent $ vcat $ map renderSmtpCheckOption opts
       , rbrace ]

renderSmtpCheckOption :: SmtpCheckOption -> Doc
renderSmtpCheckOption (SmtpConnectTimeout   i) = renderConnectTimeout i
renderSmtpCheckOption (SmtpHost             h) = renderHost h
renderSmtpCheckOption (SmtpRetry            r) = text "retry" <+> integer r
renderSmtpCheckOption (SmtpDelayBeforeRetry d) = text "delay_before_retry" <+> integer d
renderSmtpCheckOption (SmtpHeloName         n) = text "helo_name" <+> text n

renderConnectTimeout :: Integer -> Doc
renderConnectTimeout = (text "connect_timeout" <+>) . integer
renderConnectPort    :: Integer -> Doc
renderConnectPort    = (text "connect_port"    <+>) . integer
renderConnectIp      :: IPAddr -> Doc
renderConnectIp      = (text "connect_ip"    <+>)   . text . show
renderBindto         :: IPAddr -> Doc
renderBindto         = (text "bindto"          <+>) . text . show

renderHost :: Host -> Doc
renderHost (Host ip port bindto) =
  vcat [ text "host" <+> lbrace
       , indent $ vcat [ ip'
                       , port'
                       , bindto' ]
       , rbrace ]
  where ip'     = renderMaybe renderConnectIp ip
        port'   = renderMaybe renderConnectPort port
        bindto' = renderMaybe renderBindto bindto

renderMiscCheck :: MiscCheck -> Doc
renderMiscCheck (MiscCheck path timeoutM dynamicM) =
  vcat [ text "MISC_CHECK" <+> lbrace
       , indent $ vcat [ path'
                       , timeout'
                       , dynamic' ]
       , rbrace ]
  where path'    = text "misc_path" <+> doubleQuotes (text path)
        timeout' = renderMaybe ((text "misc_timeout" <+>) . integer) timeoutM
        dynamic' = renderMaybe (const (text "misc_dynamic")) dynamicM

renderVrrpNotify :: VrrpNotify -> Doc
renderVrrpNotify (NotifyMaster s) = text "notify_master" <+> text s
renderVrrpNotify (NotifyBackup s) = text "notify_backup" <+> text s
renderVrrpNotify (NotifyFault  s) = text "notify_fault"  <+> text s
renderVrrpNotify (Notify       s) = text "notify"        <+> text s

renderRealServerNotify :: RealServerNotify -> Doc
renderRealServerNotify (NotifyUp   s) = text "notify_up"   <+> text s
renderRealServerNotify (NotifyDown s) = text "notify_down" <+> text s

-- common utilities
indent :: Doc -> Doc
indent = nest 2

renderMaybe :: (a -> Doc) -> Maybe a -> Doc
renderMaybe = maybe empty
