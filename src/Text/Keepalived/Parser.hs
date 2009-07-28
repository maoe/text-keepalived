{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Text.Keepalived.Parser
  ( -- * Top-level parsers
    pKeepalivedConf
  , pKeepalivedConfType
    -- * Internal parsers
    -- ** Global Definitions
  , pGlobalDefs
    -- ** Static routes/ip addresses
  , pStaticRoutes
  , pStaticIpaddress
    -- ** VRRP scrpts
  , pVrrpScript
    -- ** VRRP sync groups
  , pVrrpSyncGroup
    -- ** VRRP instances
  , pVrrpInstance
    -- ** Virtual server groups
  , pVirtualServerGroup
    -- ** Virtual servers
  , pVirtualServer
  ) where
import Control.Applicative hiding ((<|>), many, optional)
import Data.Maybe
import Control.Monad.Identity
import Text.Keepalived.Lexer
import Text.Parsec hiding (satisfy, token, tokens)
import Text.Parsec.Pos
import Text.Parsec.Perm
import Text.Parsec.String
import qualified Text.Parsec.Prim as P
import Text.Keepalived.Types
import Network.Layer3
import Network.Layer4

-- | Parses whole keepalived.conf
pKeepalivedConf :: Stream s Identity Token => Parsec s u KeepalivedConf
pKeepalivedConf = KeepalivedConf <$> many1 pKeepalivedConfType <* eof

-- | Parses top-level directives on keepalived.conf
pKeepalivedConfType :: Stream s Identity Token => Parsec s u KeepalivedConfType
pKeepalivedConfType = choice [ TGlobalDefs         <$> pGlobalDefs
                             , TStaticRoutes       <$> pStaticRoutes
                             , TStaticIpaddress    <$> pStaticIpaddress
                             , TVrrpScript         <$> pVrrpScript
                             , TVrrpSyncGroup      <$> pVrrpSyncGroup
                             , TVrrpInstance       <$> pVrrpInstance
                             , TVirtualServerGroup <$> pVirtualServerGroup
                             , TVirtualServer      <$> pVirtualServer
                             ]


-- GLOBAL CONFIGURATION
-- | Parses @global_defs@.
pGlobalDefs :: Stream s Identity Token => Parsec s u GlobalDefs
pGlobalDefs = do
  blockId "global_defs"
  braces $ permute $ GlobalDefs <$?> ([], pNotificationEmail)
                                <|?> (Nothing, Just <$> pNotificationEmailFrom)
                                <|?> (Nothing, Just <$> pSmtpServer)
                                <|?> (Nothing, Just <$> pSmtpConnectTimeout)
                                <|?> (Nothing, Just <$> pRouterId)

-- | Parses @static_routes@.
pStaticRoutes :: Stream s Identity Token => Parsec s u [Route]
pStaticRoutes = do
  blockId "static_routes"
  braces $ many1 (pRoute <|> pBlackhole)

-- | Parses a routing entry.
pRoute :: Stream s Identity Token => Parsec s u Route
pRoute = do
  src    <- optionMaybe $ value (string "src")    >> value pIPAddr
  optional $ value $ string "to"
  dst    <- value pCIDR
  via    <- optionMaybe $ value (string "via" <|> string "gw")  >> value pIPAddr
  or     <- optionMaybe $ value (string "or")     >> value pIPAddr
  dev    <- optionMaybe $ value (string "dev")    >> value (many1 anyChar)
  scope  <- optionMaybe $ value (string "scope")  >> value (many1 anyChar)
  table  <- optionMaybe $ value (string "table")  >> value (many1 anyChar)
  metric <- optionMaybe $ value (string "metric") >> value integer
  lookAhead $ choice [ () <$ closeBrace
                     , () <$ value pCIDR
                     , () <$ value pIPAddr
                     , () <$ value (string "src" <|> string "blackhole")
                     ]
  return $ Route src dst via or dev scope table metric

-- | Parses a black hole filtering entry.
pBlackhole :: Stream s Identity Token => Parsec s u Route
pBlackhole = do
  value (string "blackhole")
  cidr <- value pCIDR
  return $ Blackhole cidr

-- | Parses a @static_ipaddress@.
pStaticIpaddress :: Stream s Identity Token => Parsec s u [Ipaddress]
pStaticIpaddress = do
  blockId "static_ipaddress"
  braces $ many1 pIpaddress
               

-- VRRP CONFIGURATION
-- Parses a @vrrp_scripts@.
pVrrpScript :: Stream s Identity Token => Parsec s u VrrpScript
pVrrpScript = do
  blockId "vrrp_script"
  ident <- value stringLiteral
  braces $ permute $ VrrpScript <$$> return ident
                                <||> pScript
                                <||> pScriptInterval
                                <|?> (Nothing, Just <$> pScriptWeight)

-- | Parses a @script@.
pScript :: Stream s Identity Token => Parsec s u String
pScript = do
  identifier "script"
  quoted stringLiteral

-- | Parsers an @interval@.
pScriptInterval :: Stream s Identity Token => Parsec s u Integer
pScriptInterval = do
  identifier "interval"
  value natural

-- | Parses a @weight@.
pScriptWeight :: Stream s Identity Token => Parsec s u Integer
pScriptWeight = do
  identifier "weight"
  value integer

-- vrrp syncronization group(s)
-- | Parses a @vrrp_sync_group@.
pVrrpSyncGroup :: Stream s Identity Token => Parsec s u VrrpSyncGroup
pVrrpSyncGroup = do
  blockId "vrrp_sync_group"
  ident <- value stringLiteral
  braces $ permute $ VrrpSyncGroup <$$> return ident
                                   <||> pVrrpGroups
                                   <|?> ([], many1 pVrrpNotify)
                                   <|?> (Nothing, Just <$> pSmtpAlert)

-- | Parses a @group@.
pVrrpGroups :: Stream s Identity Token => Parsec s u [String]
pVrrpGroups = do
  blockId "group"
  braces $ many $ value stringLiteral

-- | Parses a @vrrp instance@.
pVrrpInstance :: Stream s Identity Token => Parsec s u VrrpInstance
pVrrpInstance = do
  blockId "vrrp_instance"
  name <- value $ many1 anyChar
  braces $ permute $ VrrpInstance <$$> return name
                                  <||> pInterface
                                  <||> pVirtualRouterId
                                  <||> pPriority
                                  <|?> ([], pVirtualIpaddress)
                                  <|?> ([], pVirtualIpaddressExcluded)
                                  <|?> ([], pTrackInterface)
                                  <|?> ([], pTrackScript)
                                  <|?> ([], pVirtualRoutes)
                                  <|?> ([], many1 pVrrpNotify)
                                  <|?> (Nothing, Just <$> pVrrpState)
                                  <|?> (Nothing, Just <$> pSmtpAlert)
                                  <|?> (Nothing, Just <$> pDontTrackPrimary)
                                  <|?> (Nothing, Just <$> pMcastSrcIp)
                                  <|?> (Nothing, Just <$> pLvsSyncDaemonInterface)
                                  <|?> (Nothing, Just <$> pGarpMasterDelay)
                                  <|?> (Nothing, Just <$> pAdvertInt)
                                  <|?> (Nothing, Just <$> pAuth)
                                  <|?> (Nothing, Just <$> pNoPreempt)
                                  <|?> (Nothing, Just <$> pPreemptDelay)
                                  <|?> (Nothing, Just <$> pDebug)


-- LVS CONFIGURATION
-- | Parses a @virtual_server_group@.
pVirtualServerGroup :: Stream s Identity Token => Parsec s u VirtualServerGroup
pVirtualServerGroup = do
  blockId "virtual_server_group"
  name <- value stringLiteral
  braces $ VirtualServerGroup <$> pure name
                              <*> many pVirtualServerGroupMember

-- | Parses an entry at @virtual_server_group@.
pVirtualServerGroupMember :: Stream s Identity Token => Parsec s u VirtualServerGroupMember
pVirtualServerGroupMember = pRange <|> pAddr <|> pFwmark
   where pAddr   = VirtualServerIPAddress <$> value pIPAddr <*> value integer
         pFwmark = identifier "fwmark" >> VirtualServerFwmark <$> value integer
         pRange  = do
           (ip, diff) <- value $ do { ip' <- pIPAddr; char '-'; diff' <- natural; return (ip', diff') }
           port <- value integer
           return $ VirtualServerIPRange ip diff port

-- | Parses a @virtual_server@.
pVirtualServer :: Stream s Identity Token => Parsec s u VirtualServer
pVirtualServer = do
  blockId "virtual_server"
  ident <- pVirtualServerId
  braces $ permute $ VirtualServer <$$> return ident
                                   <||> pLvsMethod
                                   <||> pLvsSched
                                   <||> pProtocol
                                   <||> many1 pRealServer
                                   <|?> (Nothing, Just <$> pSorryServer)
                                   <|?> (Nothing, Just <$> pDelayLoop)
                                   <|?> (Nothing, Just <$> pVirtualHost)
                                   <|?> (Nothing, Just <$> pPersistenceTimeout)
                                   <|?> (Nothing, Just <$> pPersistenceGranularity)
                                   <|?> (Nothing, Just <$> pHaSuspend)
                                   <|?> (Nothing, Just <$> pAlpha)
                                   <|?> (Nothing, Just <$> pOmega)
                                   <|?> (Nothing, Just <$> pQuorum)
                                   <|?> (Nothing, Just <$> pHysteresis)
                                   <|?> (Nothing, Just <$> pQuorumUp)
                                   <|?> (Nothing, Just <$> pQuorumDown)
                                   <|?> (Nothing, Just <$> pNatMask)

-- | Parses a @nat_mask@.
pNatMask :: Stream s Identity Token => Parsec s u Netmask
pNatMask = do
  identifier "nat_mask"
  value pNetmaskOctets

-- |
pVirtualServerId :: Stream s Identity Token => Parsec s u VirtualServerId
pVirtualServerId = choice [ VirtualServerIpId     <$> pRealServerAddress
                          , VirtualServerFwmarkId <$> pFmark
                          , VirtualServerGroupId  <$> pGroupId ]

pFmark :: Stream s Identity Token => Parsec s u Integer
pFmark = do
  identifier "fwmark"
  value natural

pGroupId :: Stream s Identity Token => Parsec s u String
pGroupId = do
  blockId "group"
  value stringLiteral

pRealServerAddress :: Stream s Identity Token => Parsec s u RealServerAddress
pRealServerAddress = RealServerAddress <$> value pIPAddr <*> maybePort
  where maybePort = optionMaybe $ value (PortNumber . read <$> many1 digit)

-- | Parses a @real_server@ block.
pRealServer :: Stream s Identity Token => Parsec s u RealServer
pRealServer = do
  blockId "real_server"
  addr <- pRealServerAddress
  braces $ permute $ RealServer <$$> return addr
                                <||> pWeight
                                <|?> (Nothing, Just <$> pInhibitOnFailure)
                                <|?> (Nothing, Just <$> pHttpGet)
                                <|?> (Nothing, Just <$> pTcpCheck)
                                <|?> (Nothing, Just <$> pSmtpCheck)
                                <|?> (Nothing, Just <$> pMiscCheck)
                                <|?> ([], many1 pRealServerNotify)

-- | Parses a @HTTP_GET@ block.
pHttpGet :: Stream s Identity Token => Parsec s u HttpGet
pHttpGet = do
  blockId "HTTP_GET" <|> blockId "SSL_GET"
  braces $ permute $ HttpGet <$$> many1 pUrl
                             <|?> (Nothing, Just <$> pConnectPort)
                             <|?> (Nothing, Just <$> pBindto)
                             <|?> (Nothing, Just <$> pConnectTimeout)
                             <|?> (Nothing, Just <$> pNbGetRetry)
                             <|?> (Nothing, Just <$> pDelayBeforeRetry)

-- | Parses an @url@ block.
pUrl :: Stream s Identity Token => Parsec s u Url
pUrl = do
  blockId "url"
  braces $ permute $ Url <$$> pUrlPath
                         <|?> (Nothing, Just <$> pUrlDigest)
                         <|?> (Nothing, Just <$> pUrlStatusCode)

-- | Parses a @path@ identifier.
pUrlPath :: Stream s Identity Token => Parsec s u String
pUrlPath = do
  identifier "path"
  value stringLiteral

-- | Parses a @digest@.
pUrlDigest :: Stream s Identity Token => Parsec s u String
pUrlDigest = do
  identifier "digest"
  value stringLiteral

-- | Parses a @status_code@.
pUrlStatusCode :: Stream s Identity Token => Parsec s u Integer
pUrlStatusCode = do
  identifier "status_code"
  value natural

-- | Parses a @TCP_CHECK@.
pTcpCheck :: Stream s Identity Token => Parsec s u TcpCheck
pTcpCheck = do
  blockId "TCP_CHECK"
  braces $ permute $ TcpCheck <$$> pConnectTimeout
                              <|?> (Nothing, Just <$> pConnectPort)
                              <|?> (Nothing, Just <$> pBindto)

-- | Parses a @SMTP_CHECK@.
pSmtpCheck :: Stream s Identity Token => Parsec s u SmtpCheck
pSmtpCheck = do
  blockId "SMTP_CHECK"
  braces $ SmtpCheck <$> many pSmtpCheckOption

pSmtpCheckOption :: Stream s Identity Token => Parsec s u SmtpCheckOption
pSmtpCheckOption =
  choice [ SmtpConnectTimeout   <$> pConnectTimeout
         , SmtpHost             <$> pHost
         , SmtpRetry            <$> pRetry
         , SmtpDelayBeforeRetry <$> pDelayBeforeRetry
         , SmtpHeloName         <$> pSmtpHeloName ]

-- | Parses a @host@.
pHost :: Stream s Identity Token => Parsec s u Host
pHost = do
  blockId "host"
  braces $ permute $ Host <$?> (Nothing, Just <$> pHostIp)
                          <|?> (Nothing, Just <$> pConnectPort)
                          <|?> (Nothing, Just <$> pBindto)

pHostIp :: Stream s Identity Token => Parsec s u IPAddr
pHostIp = do
  identifier "connect_ip"
  value pIPAddr

pMiscCheck :: Stream s Identity Token => Parsec s u MiscCheck
pMiscCheck = do
  blockId "MISC_CHECK"
  braces $ permute $ MiscCheck <$$> pMiscPath
                               <|?> (Nothing, Just <$> pMiscTimeout)
                               <|?> (Nothing, Just <$> pMiscDynamic)


-- utils
satisfy :: Stream s Identity Token => (Token -> Bool) -> Parsec s u Token
satisfy f = P.token (show . snd) fst (\t -> if f t then Just t else Nothing)

match :: TokenType -> Token -> Bool
match tt (_, tt') = tt == tt'

identifier :: Stream s Identity Token => String -> Parsec s u Token
identifier = satisfy . match . Identifier

blockId :: Stream s Identity Token => String -> Parsec s u Token
blockId = satisfy . match . BlockId

openBrace :: Stream s Identity Token => Parsec s u Token
openBrace = satisfy $ match OpenBrace

closeBrace :: Stream s Identity Token => Parsec s u Token
closeBrace = satisfy $ match CloseBrace

braces :: Stream s Identity Token => Parsec s u r -> Parsec s u r
braces = between openBrace closeBrace

value :: Stream s Identity Token => Parsec String () a -> ParsecT s u Identity a
value p = do
  tok <- satisfy match
  case parse p (sourceName $ fst tok) (unValue $ snd tok) of
    Right r  -> return r
    Left err -> error $ show err
  where match :: Token -> Bool
        match (_, Value s)
          | Right _  <- parse (p <* eof) "" s = True
          | otherwise                         = False
        match _                               = False

quoted :: Stream s Identity Token => Parsec String () a -> ParsecT s u Identity a
quoted p = do
  tok <- satisfy match
  case parse p (sourceName $ fst tok) (unQuoted $ snd tok) of
    Right r  -> return r
    Left err -> error $ show err
  where match :: Token -> Bool
        match (_, Quoted s)
          | Right _  <- parse (p <* eof) "" s = True
          | otherwise                         = False
        match _                               = False

-- blocks
pSmtpServer :: Stream s Identity Token => Parsec s u IPAddr
pSmtpServer = do
  identifier "smtp_server"
  value pIPAddr

pVrrpState :: Stream s Identity Token => Parsec s u VrrpState
pVrrpState = do
  identifier "state"
  pViMaster <|> pViBackup
  where pViMaster = value $ string "MASTER" >> return VrrpMaster
        pViBackup = value $ string "BACKUP" >> return VrrpBackup

pVirtualRouterId :: Stream s Identity Token => Parsec s u Vrid
pVirtualRouterId = do
  identifier "virtual_router_id"
  value $ Vrid . read <$> many1 digit

pPriority :: Stream s Identity Token => Parsec s u Priority
pPriority = do
  identifier "priority"
  value $ Priority . read <$> many1 digit

pIpaddress :: Stream s Identity Token => Parsec s u Ipaddress
pIpaddress = do
  dst <- value pCIDR
  brd   <- optionMaybe $ value (string "brd")   >> value pIPAddr
  dev   <- optionMaybe $ value (string "dev")   >> value stringLiteral
  scope <- optionMaybe $ value (string "scope") >> value stringLiteral
  label <- optionMaybe $ value (string "label") >> value stringLiteral
  lookAhead $ () <$ closeBrace <|> () <$ value pCIDR
  return $ Ipaddress dst brd dev scope label

pVirtualIpaddress :: Stream s Identity Token => Parsec s u [Ipaddress]
pVirtualIpaddress = do
  blockId "virtual_ipaddress"
  braces $ many1 pIpaddress

pVirtualIpaddressExcluded :: Stream s Identity Token => Parsec s u [Ipaddress]
pVirtualIpaddressExcluded = do
  blockId "virtual_ipaddress_excluded"
  braces $ many1 pIpaddress

pVrrpNotify :: Stream s Identity Token => Parsec s u VrrpNotify
pVrrpNotify =
  choice [ identifier "notify"        >> choice [ value  (Notify       <$> many1 anyChar)
                                                , quoted (Notify       <$> many1 anyChar) ]
         , identifier "notify_master" >> choice [ value  (NotifyMaster <$> many1 anyChar)
                                                , quoted (NotifyMaster <$> many1 anyChar) ]
         , identifier "notify_backup" >> choice [ value  (NotifyBackup <$> many1 anyChar)
                                                , quoted (NotifyBackup <$> many1 anyChar) ]
         , identifier "notify_fault"  >> choice [ value  (NotifyFault  <$> many1 anyChar)
                                                , quoted (NotifyFault  <$> many1 anyChar) ]
         ]

pRealServerNotify :: Stream s Identity Token => Parsec s u RealServerNotify
pRealServerNotify =
  choice [ identifier "notify_up"   >> choice [ value  (NotifyUp   <$> many1 anyChar)
                                              , quoted (NotifyUp   <$> many1 anyChar) ]
         , identifier "notify_down" >> choice [ value  (NotifyDown <$> many1 anyChar)
                                              , quoted (NotifyDown <$> many1 anyChar) ]]

pAuth :: Stream s Identity Token => Parsec s u Auth
pAuth = do
  blockId "authentication"
  braces $ permute $ Auth <$$> pAuthType <||> pAuthPass

pAuthType :: Stream s Identity Token => Parsec s u AuthType
pAuthType = do
  identifier "auth_type"
  pPASS <|> pAH
  where pPASS = value $ string "PASS" >> return PASS
        pAH   = value $ string "AH"   >> return AH

pLvsMethod :: Stream s Identity Token => Parsec s u LvsMethod
pLvsMethod = do
  identifier "lvs_method" <|> identifier "lb_kind"
  choice [ value (string "DR")  >> return DR
         , value (string "TUN") >> return TUN
         , value (string "NAT") >> return NAT ]

pLvsSched :: Stream s Identity Token => Parsec s u LvsSched
pLvsSched = do
  identifier "lvs_sched" <|> identifier "lb_algo"
  choice [ value (try (string "rr"))   >> return RR
         , value (try (string "wrr"))  >> return WRR
         , value (try (string "lc"))   >> return LC
         , value (try (string "wlc"))  >> return WLC
         , value (try (string "lblc")) >> return LBLC
         , value (try (string "sh"))   >> return SH
         , value (try (string "dh"))   >> return DH ]

pProtocol :: Stream s Identity Token => Parsec s u Protocol
pProtocol = do
  identifier "protocol"
  choice [ value (string "TCP") >> return TCP
         , value (string "UDP") >> return UDP ]

-- CIDR parsers
pSorryServer :: Stream s Identity Token => Parsec s u L4Addr
pSorryServer = do
  identifier "sorry_server"
  L4Addr <$> value pIPAddr <*> value (PortNumber . read <$> many1 digit)

-- IP Address parsers
pMcastSrcIp :: Stream s Identity Token => Parsec s u IPAddr
pMcastSrcIp = do
  identifier "mcast_src_ip"
  value pIPAddr

pBindto :: Stream s Identity Token => Parsec s u IPAddr
pBindto = do
  identifier "bindto"
  value pIPAddr

-- Integer parsers
natural :: Parser Integer
natural = read <$> many1 digit
integer :: Parser Integer
integer = do
  sign <- option id (negate <$ char '-')
  sign . read <$> many1 digit

pGarpMasterDelay :: Stream s Identity Token => Parsec s u Integer
pGarpMasterDelay = do
  identifier "garp_master_delay"
  value natural

pAdvertInt :: Stream s Identity Token => Parsec s u Integer
pAdvertInt = do
  identifier "advert_int"
  value natural

pPreemptDelay :: Stream s Identity Token => Parsec s u Integer
pPreemptDelay = do
  identifier "preempt_delay"
  value natural

pWeight :: Stream s Identity Token => Parsec s u Integer
pWeight = do
  identifier "weight"
  value natural

pConnectPort :: Stream s Identity Token => Parsec s u Integer
pConnectPort = do
  identifier "connect_port"
  value natural

pConnectTimeout :: Stream s Identity Token => Parsec s u Integer
pConnectTimeout = do
  identifier "connect_timeout"
  value natural

pSmtpConnectTimeout :: Stream s Identity Token => Parsec s u Integer
pSmtpConnectTimeout = do
  identifier "smtp_connect_timeout"
  value natural

pMiscTimeout :: Stream s Identity Token => Parsec s u Integer
pMiscTimeout = do
  identifier "misc_timeout"
  value natural

pNbGetRetry :: Stream s Identity Token => Parsec s u Integer
pNbGetRetry = do
  identifier "nb_get_retry"
  value natural

pRetry :: Stream s Identity Token => Parsec s u Integer
pRetry = do
  identifier "retry"
  value natural

pDelayBeforeRetry :: Stream s Identity Token => Parsec s u Integer
pDelayBeforeRetry = do
  identifier "delay_before_retry"
  value natural

pDelayLoop :: Stream s Identity Token => Parsec s u Integer
pDelayLoop = do
  identifier "delay_loop"
  value natural

pPersistenceTimeout :: Stream s Identity Token => Parsec s u Integer
pPersistenceTimeout = do
  identifier "persistence_timeout"
  value natural

pQuorum :: Stream s Identity Token => Parsec s u Integer
pQuorum = do
  identifier "quorum"
  value natural

pHysteresis :: Stream s Identity Token => Parsec s u Integer
pHysteresis = do
  identifier "hysteresis"
  value natural

-- [String] parsers
pTrackInterface :: Stream s Identity Token => Parsec s u [TrackInterface]
pTrackInterface = do
  blockId "track_interface"
  braces $ many1 $ do
    iface  <- value stringLiteral
    weight <- optionMaybe $ identifier "weight" >> value integer
    return $ TrackInterface iface weight

pTrackScript :: Stream s Identity Token => Parsec s u [TrackScript]
pTrackScript = do
  blockId "track_script"
  braces $ many1 $ do
    script <- value stringLiteral
    weight <- optionMaybe $ identifier "weight" >> value integer
    return $ TrackScript script weight

pVirtualRoutes :: Stream s Identity Token => Parsec s u [Route]
pVirtualRoutes = do
  blockId "virtual_routes"
  braces $ many1 (pRoute <|> pBlackhole)

pNotificationEmail :: Stream s Identity Token => Parsec s u [String]
pNotificationEmail = do
  blockId "notification_email"
  braces $ many $ value stringLiteral


-- String parsers
stringLiteral :: Parser String
stringLiteral = many1 anyChar

pInterface :: Stream s Identity Token => Parsec s u String
pInterface = do
  identifier "interface"
  value stringLiteral

pLvsSyncDaemonInterface :: Stream s Identity Token => Parsec s u String
pLvsSyncDaemonInterface = do
  identifier "lvs_sync_daemon_interface"
  value stringLiteral

pAuthPass :: Stream s Identity Token => Parsec s u String
pAuthPass = do
  identifier "auth_pass"
  value stringLiteral

pSmtpHeloName :: Stream s Identity Token => Parsec s u String
pSmtpHeloName = do
  identifier "helo_name"
  value stringLiteral <|> quoted stringLiteral

pMiscPath :: Stream s Identity Token => Parsec s u String
pMiscPath = do
  identifier "misc_path"
  value stringLiteral <|> quoted stringLiteral

pPersistenceGranularity :: Stream s Identity Token => Parsec s u String
pPersistenceGranularity = do
  identifier "persistence_granularity"
  value stringLiteral

pVirtualHost :: Stream s Identity Token => Parsec s u String
pVirtualHost = do
  identifier "virtualhost"
  value stringLiteral

pNotificationEmailFrom :: Stream s Identity Token => Parsec s u String
pNotificationEmailFrom = do
  identifier "notification_email_from"
  value stringLiteral

pRouterId :: Stream s Identity Token => Parsec s u String
pRouterId = do
  identifier "router_id" <|> identifier "lvs_id"
  value stringLiteral

pQuorumUp :: Stream s Identity Token => Parsec s u String
pQuorumUp = do
  identifier "quorum_up"
  value stringLiteral

pQuorumDown :: Stream s Identity Token => Parsec s u String
pQuorumDown = do
  identifier "quorum_down"
  value stringLiteral


-- unit parsers
pHaSuspend :: Stream s Identity Token => Parsec s u ()
pHaSuspend = () <$ identifier "ha_suspend"

pAlpha :: Stream s Identity Token => Parsec s u ()
pAlpha = () <$ identifier "alpha"

pOmega :: Stream s Identity Token => Parsec s u ()
pOmega = () <$ identifier "omega"

pInhibitOnFailure :: Stream s Identity Token => Parsec s u ()
pInhibitOnFailure = () <$ identifier "inhibit_on_failure"

pDontTrackPrimary :: Stream s Identity Token => Parsec s u ()
pDontTrackPrimary = () <$ identifier "dont_track_primary"

pNoPreempt :: Stream s Identity Token => Parsec s u ()
pNoPreempt = () <$ identifier "nopreempt"

pDebug :: Stream s Identity Token => Parsec s u ()
pDebug = () <$ identifier "debug"

pMiscDynamic :: Stream s Identity Token => Parsec s u ()
pMiscDynamic = () <$ identifier "misc_dynamic"

pSmtpAlert :: Stream s Identity Token => Parsec s u ()
pSmtpAlert = () <$ identifier "smtp_alert"
