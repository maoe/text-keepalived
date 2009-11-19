 module Instances where
import Control.Applicative
import Data.Char
import Text.Keepalived.Lexer
import Text.Keepalived.Types
import Test.QuickCheck.Instances
import Test.QuickCheck
import Text.Parsec.Pos
import Network.Layer3
import Network.Layer4

-- * Arbitrary instances (generators)
-- | keepalived.conf
instance Arbitrary KeepalivedConf where
  arbitrary = KeepalivedConf <$> listOf1 arbitrary

instance Arbitrary KeepalivedConfType where
  arbitrary = oneof [ TGlobalDefs <$> arbitrary
                    , TStaticRoutes <$> listOf arbitrary
                    , TStaticIpaddress <$> listOf arbitrary
                    , TVrrpScript <$> arbitrary
                    , TVrrpSyncGroup <$> arbitrary
                    , TVrrpInstance <$> arbitrary
                    , TVirtualServerGroup <$> arbitrary
                    , TVirtualServer <$> arbitrary
                    ]

-- | global_defs generator
instance Arbitrary GlobalDefs where
  arbitrary = GlobalDefs <$> listOf   arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary

-- | routing generator
instance Arbitrary Route where
  arbitrary = oneof [ blackhole, route ]
    where blackhole = Blackhole <$> arbitrary
          route     = Route <$> maybeGen arbitrary
                            <*> arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen (listOf1 lowerAlpha)
                            <*> maybeGen scope
                            <*> maybeGen link
                            <*> maybeGen positive
          scope = oneof [ pure "host", pure "link"
                        , pure "global", listOf1 numeric ]
          link = oneof [ pure "local", pure "main"
                       , pure "default", pure "all"
                       , listOf1 numeric ]

-- | Ipaddress generator
instance Arbitrary Ipaddress where
  arbitrary = Ipaddress <$> arbitrary
                        <*> maybeGen arbitrary
                        <*> maybeGen (listOf1 lowerAlpha)
                        <*> maybeGen scope
                        <*> maybeGen (listOf1 lowerAlpha)
    where scope = oneof [ pure "host", pure "link"
                        , pure "global", listOf1 numeric ]

-- | vrrp_script generator
instance Arbitrary VrrpScript where
  arbitrary = VrrpScript <$> listOf1 lowerAlpha
                         <*> arbitrary
                         <*> arbitrary
                         <*> maybeGen arbitrary

-- | vrrp_sync_group generator
instance Arbitrary VrrpSyncGroup where
  arbitrary = VrrpSyncGroup <$> arbitrary
                            <*> listOf arbitrary
                            <*> arbitrary
                            <*> arbitrary

-- | vrrp_notify generator
instance Arbitrary VrrpNotify where
  arbitrary = oneof [ NotifyMaster <$> listOf1 nonSpacePrintableAscii
                    , NotifyBackup <$> listOf1 nonSpacePrintableAscii
                    , NotifyFault  <$> listOf1 nonSpacePrintableAscii
                    , Notify       <$> listOf1 nonSpacePrintableAscii
                    ]

-- | vrrp_instance
instance Arbitrary VrrpInstance where
  arbitrary = VrrpInstance <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

-- | virtual_router_id generator
instance Arbitrary Vrid where
  arbitrary = Vrid <$> arbitrary

-- | priority generator
instance Arbitrary Priority where
  arbitrary = Priority <$> arbitrary

instance Arbitrary TrackInterface where
  arbitrary = TrackInterface <$> arbitrary
                             <*> maybeGen arbitrary

instance Arbitrary TrackScript where
  arbitrary = TrackScript <$> arbitrary
                          <*> maybeGen arbitrary

instance Arbitrary VrrpState where
  arbitrary = oneof [ pure VrrpMaster
                    , pure VrrpBackup ]

instance Arbitrary Auth where
  arbitrary = Auth <$> arbitrary <*> arbitrary

instance Arbitrary AuthType where
  arbitrary = oneof [ pure PASS, pure AH ]

-- | virtual_server_group generator
instance Arbitrary VirtualServerGroup where
  arbitrary = VirtualServerGroup <$> arbitrary
                                 <*> listOf1 arbitrary

instance Arbitrary VirtualServerGroupMember where
  arbitrary = oneof [ VirtualServerIPAddress <$> arbitrary <*> arbitrary
                    , VirtualServerIPRange <$> arbitrary <*> arbitrary <*> arbitrary
                    , VirtualServerFwmark <$> arbitrary
                    ]

instance Arbitrary VirtualServer where
  arbitrary = VirtualServer <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> listOf1 arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary
                            <*> maybeGen arbitrary

instance Arbitrary VirtualServerId where
  arbitrary = oneof [ VirtualServerIpId <$> arbitrary
                    , VirtualServerFwmarkId <$> arbitrary
                    , VirtualServerGroupId <$> arbitrary
                    ]

instance Arbitrary RealServerAddress where
  arbitrary = RealServerAddress <$> arbitrary
                                <*> maybeGen arbitrary

instance Arbitrary PortNumber where
  arbitrary = PortNumber <$> arbitrary

instance Arbitrary L4Addr where
  arbitrary = L4Addr <$> arbitrary <*> arbitrary

instance Arbitrary RealServer where
  arbitrary = RealServer <$> arbitrary
                         <*> arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> maybeGen arbitrary
                         <*> listOf arbitrary

instance Arbitrary HttpGet where
  arbitrary = oneof [ HttpGet <$> listOf1 arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                    , SslGet  <$> listOf1 arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                              <*> maybeGen arbitrary
                    ]

instance Arbitrary Url where
  arbitrary = Url <$> arbitrary
                  <*> maybeGen arbitrary
                  <*> maybeGen arbitrary

instance Arbitrary TcpCheck where
  arbitrary = TcpCheck <$> arbitrary
                       <*> maybeGen arbitrary
                       <*> maybeGen arbitrary

instance Arbitrary SmtpCheck where
  arbitrary = SmtpCheck <$> listOf arbitrary

instance Arbitrary SmtpCheckOption where
  arbitrary = oneof [ SmtpConnectTimeout <$> arbitrary
                    , SmtpHost <$> arbitrary
                    , SmtpRetry <$> arbitrary
                    , SmtpDelayBeforeRetry <$> arbitrary
                    , SmtpHeloName <$> arbitrary
                    ]

instance Arbitrary Host where
  arbitrary = Host <$> maybeGen arbitrary
                   <*> maybeGen arbitrary
                   <*> maybeGen arbitrary

instance Arbitrary RealServerNotify where
  arbitrary = oneof [ NotifyUp <$> arbitrary
                    , NotifyDown <$> arbitrary
                    ]

instance Arbitrary MiscCheck where
  arbitrary = MiscCheck <$> arbitrary
                        <*> maybeGen arbitrary
                        <*> maybeGen arbitrary

instance Arbitrary Protocol where
  arbitrary = oneof [ pure TCP, pure UDP ]

instance Arbitrary LvsSched where
  arbitrary = oneof [ pure RR
                    , pure WRR
                    , pure LC
                    , pure WLC
                    , pure LBLC
                    , pure SH
                    , pure DH
                    ]

instance Arbitrary LvsMethod where
  arbitrary = oneof [ pure NAT
                    , pure DR
                    , pure TUN
                    ]

-- | TokenType generator
instance Arbitrary TokenType where
  arbitrary = oneof [ Identifier <$> elements identifiers
                    , BlockId    <$> elements blockIdentifiers
                    , pure OpenBrace
                    , pure CloseBrace
                    , Included   <$> nonEmpty arbitrary
                    , Quoted     <$> listOf1 printableAscii
                    , Value      <$> listOf1 nonSpacePrintableAscii
                    ]

-- | SourcePos generator
instance Arbitrary SourcePos where
  arbitrary = newPos <$> listOf1 nonSpacePrintableAscii
                     <*> arbitrary
                     <*> arbitrary

-- | IPAddr generator
instance Arbitrary IPAddr where
  arbitrary = IPAddr <$> arbitrary

-- | Netmask generator
instance Arbitrary Netmask where
  arbitrary = Netmask <$> lessThan 32 positive

-- | CIDR generator
instance Arbitrary CIDR where
  arbitrary = CIDR <$> arbitrary <*> arbitrary

-- * Common utility functions


alphaNum_ :: Gen String
alphaNum_ = liftA2 (:) alphabet body
  where body = listOf $ oneof [alphabet, numeric, pure '_']

alphabet :: Gen Char
alphabet = oneof [lowerAlpha, upperAlpha]

-- | ASCII /\ printable characters
printableAscii :: Gen Char
printableAscii = arbitrary `suchThat` isPrintableAscii

-- | ASCII /\ printable /\ !space characters
nonSpacePrintableAscii :: Gen Char
nonSpacePrintableAscii = nonSpace
                           `suchThat` isPrintableAscii
                           `suchThat` liftA not isDoubleQuote

-- | Pritable ASCII
isPrintableAscii :: Char -> Bool
isPrintableAscii c = isAscii c && isPrint c

-- | Double quotation mark
isDoubleQuote :: Char -> Bool
isDoubleQuote '\"' = True
isDoubleQuote _    = False

-- | Escaped double quotation mark
escapedDoubleQuote :: Gen Char
escapedDoubleQuote = pure '\"'
