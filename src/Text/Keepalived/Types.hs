module Text.Keepalived.Types where
import Network.Layer3
import Network.Layer4
import Data.Word
import Text.PrettyPrint.HughesPJ

-- keepalived.conf
data KeepalivedConf = KeepalivedConf [KeepalivedConfType]

data KeepalivedConfType = TGlobalDefs      GlobalDefs
                        | TStaticRoutes    [Route]
                        | TStaticIpaddress [[String]]
                        | TVrrpScript      VrrpScript
                        | TVrrpSyncGroup   VrrpSyncGroup
                        | TVrrpInstance    VrrpInstance
                        | TVirtualServer   VirtualServer
                  
-- GLOBAL CONFIGURATION
-- global definitions
data GlobalDefs = GlobalDefs
  { notificationEmail     :: [String]
  , notificationEmailFrom :: Maybe String
  , smtpServer            :: Maybe IPAddr
  , smtpConnectTimeout    :: Maybe Integer
  , routerId              :: Maybe String
  }

-- static routes/addresses
data Route =
   Blackhole CIDR
 | Route { src      :: Maybe IPAddr
         , dest     :: CIDR
         , via      :: Maybe IPAddr
         , or       :: Maybe IPAddr
         , rDev     :: Maybe String
         , rScope   :: Maybe String
         , table    :: Maybe String
         , metric   :: Maybe Integer
         }

data StaticAddr = StaticAddr
  { cidr   :: CIDR
  , brd    :: IPAddr
  , aDev   :: String
  , aScope :: String }
  deriving Show

-- VRRPD CONFIGURATION
-- vrrp script
data VrrpScript = VrrpScript
  { scriptId              :: String
  , vrrpScript            :: String
  , scriptInterval        :: Integer
  , scriptWeight          :: Maybe Integer
  } deriving Show

-- vrrp synchronizations group(s)
data VrrpSyncGroup = VrrpSyncGroup
  { syncName              :: String
  , syncGroup             :: [String]
  , syncNotify            :: [Notify]
  , syncSmtpAlert         :: Maybe ()
  } deriving Show

-- vrrp instance(s)
data VrrpInstance = VrrpInstance
  { vrrpName              :: String
  , vrrpInterface         :: String
  , vrid                  :: Vrid
  , priority              :: Priority
  , virtualIpaddress      :: [CIDR]
  , virtualIpaddressExcluded :: [CIDR]
  , trackInterfaces       :: [[String]]
  , trackScript           :: [[String]]
  , virtualRoutes         :: [Route]
  , vrrpNotify            :: [Notify]
  , vrrpState             :: Maybe VrrpState
  , smtpAlert             :: Maybe ()
  , dontTrackPrimary      :: Maybe ()
  , mcastSrcIp            :: Maybe IPAddr
  , lvsSyncDaemon         :: Maybe String
  , garpMasterDelay       :: Maybe Integer
  , advertInt             :: Maybe Integer
  , authentication        :: Maybe Auth
  , noPreempt             :: Maybe ()
  , preemptDelay          :: Maybe Integer
  , debug                 :: Maybe ()
  }

data VrrpState = VrrpMaster | VrrpBackup

data Auth = Auth
  { authType              :: AuthType
  , authPass              :: String
  }

data AuthType = PASS | AH

newtype Vrid = Vrid { unVrid :: Word8 }
instance Show Vrid where
  show = show . unVrid

newtype Priority = Priority { unPriority :: Word8 }
instance Show Priority where
  show = show . unPriority

-- LVS CONFIGURATION
-- virtual server group(s)
data VirtualServerGroup = VirtualServerGroup
  { groupName              :: String
  , groupAddr              :: [CIDR]
  , groupFwmark            :: [Integer]
  } deriving Show

-- virtual server(s)
data VirtualServer = VirtualServer
  { virtualServerId        :: VirtualServerId
  , lbKind                 :: LbKind
  , lbAlgo                 :: LbAlgo
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
  }

data VirtualServerId = VirtualServerIp      L4Addr
                     | VirtualServerFwmark  Integer
                     | VirtualServerGroupId String

data LbKind = NAT | DR | TUN

data LbAlgo = RR | WRR | LC | WLC | LBLC | SH | DH

data Protocol = TCP | UDP

data RealServer = RealServer
  { realServerAddress     :: L4Addr
  , weight                :: Integer
  , inhibitOnFailure      :: Maybe ()
  , httpGet               :: Maybe HttpGet
  , tcpCheck              :: Maybe TcpCheck
  , smtpCheck             :: Maybe SmtpCheck
  , miscCheck             :: Maybe MiscCheck
  , realServerNotify      :: [Notify]
  }

data HttpGet =
  HttpGet { httpUrl               :: [Url]
          , httpPort              :: Maybe Integer
          , httpBindTo            :: Maybe IPAddr
          , httpTimeout           :: Maybe Integer
          , httpNbGetRetry        :: Maybe Integer
          , httpDelayBeforeRetry  :: Maybe Integer
          } |
  SslGet  { sslUrl                :: [Url]
          , sslPort               :: Maybe Integer
          , sslBindTo             :: Maybe IPAddr
          , sslTimeout            :: Maybe Integer
          , sslNbGetRetry         :: Maybe Integer
          , sslDelayBeforeRetry   :: Maybe Integer
          }

data Url = Url
  { urlPath               :: String
  , urlDigest             :: Maybe String
  , urlStatusCode         :: Maybe Integer
  }

data TcpCheck = TcpCheck
  { tcpTimeout            :: Integer
  , tcpPort               :: Maybe Integer
  , tcpBindTo             :: Maybe IPAddr
  }

data SmtpCheck = SmtpCheck
  { smtpHost              :: Host
  , smtpTimeout           :: Integer
  , smtpRetry             :: Maybe Integer
  , smtpDelayBeforeRetry  :: Maybe Integer
  , smtpHeloName          :: Maybe String
  }

data Host = Host
  { hostIP                :: Maybe IPAddr
  , hostPort              :: Maybe Integer
  , hostBindTo            :: Maybe IPAddr
  }

data MiscCheck = MiscCheck
  { miscPath              :: String
  , miscTimeout           :: Maybe Integer
  , miscDynamic           :: Maybe ()
  }

data Notify = NotifyMaster String
            | NotifyBackup String
            | NotifyFault  String
            | Notify       String

-- Show instnance declarations
instance Show KeepalivedConf where
  show = render . renderKeepalivedConf

instance Show KeepalivedConfType where
  show = render . renderKeepalivedConfType

instance Show GlobalDefs where
  show = render . renderGlobalDefs

instance Show Route where
  show = render . renderRoute

instance Show VrrpInstance where
  show = render . renderVrrpInstance

instance Show VrrpState where
  show = render . renderVrrpState

instance Show Auth where
  show = render . renderAuth

instance Show AuthType where
  show = render . renderAuthType

instance Show VirtualServer where
  show = render . renderVirtualServer

instance Show VirtualServerId where
  show = render . renderVirtualServerId

instance Show LbKind where
  show = render . renderLbKind

instance Show LbAlgo where
  show = render . renderLbAlgo

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

instance Show Notify where
  show = render . renderNotify

-- pretty printer
renderKeepalivedConf :: KeepalivedConf -> Doc
renderKeepalivedConf (KeepalivedConf ts) = vcat $ map renderKeepalivedConfType ts

renderKeepalivedConfType :: KeepalivedConfType -> Doc
renderKeepalivedConfType (TGlobalDefs      d) = renderGlobalDefs d
renderKeepalivedConfType (TStaticRoutes    r) = renderStaticRoutes r
renderKeepalivedConfType (TStaticIpaddress i) = text $ show i
renderKeepalivedConfType (TVrrpScript      s) = text $ show s
renderKeepalivedConfType (TVrrpSyncGroup   g) = text $ show g
renderKeepalivedConfType (TVrrpInstance    i) = renderVrrpInstance i
renderKeepalivedConfType (TVirtualServer   s) = renderVirtualServer s

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
renderStaticRoutes r =
  vcat [ text "static_routes" <+> lbrace
       , indent $ vcat $ map renderRoute r
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
renderNotificationEmail mails =
  vcat [ text "notification_email" <+> lbrace
       , indent $ vcat $ map text mails
       , rbrace]

renderVrrpInstance :: VrrpInstance -> Doc
renderVrrpInstance vi =
  vcat [ text "vrrp_instance" <+> text (vrrpName vi) <+> lbrace
       , indent $ vcat [ text "interface" <+> text (vrrpInterface vi)
                       , text "virtual_router_id" <+> text (show (vrid vi))
                       , text "priority" <+> text (show (priority vi))
                       -- , text "virtual_ipaddress" <+> renderVirtualIpaddress
                       -- , text "virtual_ipaddress_excluded" <+> renderVirtualIpaddressExcluded
                       -- , renderTrackInterfaces
                       -- , renderMaybe renderTrackInterfaces (trackInterfaces vi)
                       , renderVirtualRoutes (virtualRoutes vi)
                       ]
       , rbrace ]

renderVirtualRoutes :: [Route] -> Doc
renderVirtualRoutes rs =
  vcat [ text "virtual_routes" <+> lbrace
       , indent $ vcat (map renderRoute rs)
       , rbrace ]

renderVrrpState :: VrrpState -> Doc
renderVrrpState s = text "state" <+> state s
  where state VrrpMaster = text "master"
        state VrrpBackup = text "backup"

renderAuth :: Auth -> Doc
renderAuth (Auth typ pass) =
  vcat [ text "auth" <+> lbrace
       , text "auth_type" <+> renderAuthType typ
       , text "auth_pass" <+> text pass
       , rbrace ]

renderAuthType :: AuthType -> Doc
renderAuthType t = text "auth_type" <+> typ t
  where typ PASS = text "PASS"
        typ AH   = text "AH"

renderVirtualServer :: VirtualServer -> Doc
renderVirtualServer vs =
  vcat [ text "virtual_server" <+> renderVirtualServerId (virtualServerId vs) <+> lbrace
       , indent $ vcat [ renderLbKind (lbKind vs)
                       , renderLbAlgo (lbAlgo vs)
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
renderVirtualServerId (VirtualServerIp addr)       = text (show (l4IpAddr addr)) <+> text (show (l4Port addr))
renderVirtualServerId (VirtualServerFwmark i)      = text "fwmark" <+> integer i
renderVirtualServerId (VirtualServerGroupId ident) = text "group"  <+> text ident

renderLbKind :: LbKind -> Doc
renderLbKind k = text "lb_kind" <+> kind k
  where kind NAT = text "NAT"
        kind DR  = text "DR"
        kind TUN = text "TUN"

renderLbAlgo :: LbAlgo -> Doc
renderLbAlgo a = text "lb_algo" <+> algo a
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
renderRealServer (RealServer addr weight inhibit httpGet tcpCheck smtpCheck miscCheck notify) =
  vcat [ text "real_server" <+> addr' <+> lbrace
       , indent $ vcat [ weight'
                       , inhibit'
                       , httpGet'
                       , tcpCheck'
                       , smtpCheck'
                       , miscCheck'
                       , notify' ]
       , rbrace ]
  where addr'      = text (show (l4IpAddr addr)) <+> text (show (l4Port addr))
        weight'    = text "weight" <+> integer weight
        inhibit'   = renderMaybe (const $ text "inhibit_on_failure") inhibit
        httpGet'   = renderMaybe renderHttpGet httpGet
        tcpCheck'  = renderMaybe renderTcpCheck tcpCheck
        smtpCheck' = renderMaybe renderSmtpCheck smtpCheck
        miscCheck' = renderMaybe renderMiscCheck miscCheck
        notify'    = vcat $ map renderNotify notify

renderHttpGet :: HttpGet -> Doc
renderHttpGet (HttpGet url port bindTo timeout nb delay) =
  vcat [ text "HTTP_GET" <+> lbrace
       , indent $ vcat [ vcat (map renderUrl url)
                       , renderMaybe renderConnectPort port
                       , renderMaybe renderBindTo bindTo
                       , renderMaybe renderConnectTimeout timeout
                       , renderMaybe ((text "nb_get_retry" <+>) . integer) nb
                       , renderMaybe ((text "delay_before_retry" <+>) . integer) delay ]
       , rbrace ]
renderHttpGet (SslGet  url port bindTo timeout nb delay) =
  vcat [ text "SSL_GET" <+> lbrace
       , indent $ vcat [ vcat (map renderUrl url)
                       , renderMaybe renderConnectPort port
                       , renderMaybe renderBindTo bindTo
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
renderTcpCheck (TcpCheck timeout port bindTo) =
  vcat [ text "TCP_CHECK" <+> lbrace
       , indent $ vcat [ timeout'
                       , port'
                       , bindTo' ]
       , rbrace ]
  where timeout' = renderConnectTimeout timeout
        port'    = renderMaybe renderConnectPort port
        bindTo'  = renderMaybe renderBindTo bindTo

renderSmtpCheck :: SmtpCheck -> Doc
renderSmtpCheck (SmtpCheck host timeout retry delay helo) =
  vcat [ text "SMTP_CHECK" <+> lbrace
       , indent $ vcat [ host'
                       , timeout'
                       , retry'
                       , delay'
                       , helo' ]
       , rbrace ]
  where host'    = renderHost host
        timeout' = renderConnectTimeout timeout
        retry'   = renderMaybe ((text "retry" <+>) . integer) retry
        delay'   = renderMaybe ((text "delay_before_retry" <+>) . integer) delay
        helo'    = renderMaybe ((text "helo_name" <+>) . text) helo

renderConnectTimeout :: Integer -> Doc
renderConnectTimeout = (text "connect_timeout" <+>) . integer
renderConnectPort    :: Integer -> Doc
renderConnectPort    = (text "connect_port"    <+>) . integer
renderBindTo         :: IPAddr -> Doc
renderBindTo         = (text "bindto"          <+>) . text . show

renderHost :: Host -> Doc
renderHost (Host ip port bindTo) =
  vcat [ text "host" <+> lbrace
       , indent $ vcat [ ip'
                       , port'
                       , bindTo' ]
       , rbrace ]
  where ip'     = renderMaybe ((text "connect_ip" <+>) . text . show) ip
        port'   = renderMaybe renderConnectPort port
        bindTo' = renderMaybe renderBindTo bindTo

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

renderNotify :: Notify -> Doc
renderNotify (NotifyMaster s) = text "notify_master" <+> text s
renderNotify (NotifyBackup s) = text "notify_backup" <+> text s
renderNotify (NotifyFault  s) = text "notify_fault"  <+> text s
renderNotify (Notify       s) = text "notify"        <+> text s

-- common utilities
indent :: Doc -> Doc
indent = nest 2

renderMaybe :: (a -> Doc) -> Maybe a -> Doc
renderMaybe = maybe empty
