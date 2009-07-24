{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Kc.Option where
import Control.Applicative
import Control.Monad.Reader
import Data.Function
import Data.List
import System.Console.GetOpt
import System.Exit

import Text.Keepalived
import Text.Keepalived.Types
import Network.Layer3

-- * Application
-- | Application type
newtype App a = App { runA :: ReaderT AppConfig IO a }
              deriving (Monad, MonadIO, MonadReader AppConfig)

runApp :: App a -> AppConfig -> IO a
runApp = runReaderT . runA

-- | Show this help message
helpApp :: App ()
helpApp = liftIO $ putStrLn "help"

-- | Print version number
versionApp :: App ()
versionApp = liftIO $ putStrLn "0.0.1"

-- | Parse keepalived.conf
parseApp :: [FilePath] -> App [KeepalivedConf]
parseApp fs = liftIO $ mapM parseFromFile fs
                `catch` \e -> do { print e; exitFailure }

-- | Verify keepalived.conf
verifyApp :: App ()
verifyApp = do
  AppConfig files verbosity mode <- ask
  case verbosity of
    Verbose -> verifyVerbosely mode files
    Debug   -> verifyVerbosely mode files
    _       -> verify          mode files
  where verify          m f = parseApp f                          >> printOK >> printMode m
        verifyVerbosely m f = parseApp f >>= liftIO . mapM_ print >> printOK >> printMode m
        printOK = liftIO $ putStrLn "OK"
        printMode = liftIO . print

-- | Dump parsed keepalived.conf
dumpApp :: App ()
dumpApp = do
  path <- asks filePath
  ks <- parseApp path
  liftIO $ mapM_ print ks

-- | Search specified VIP, RIP or VRID from keepalived.conf
searchApp :: App ()
searchApp = do
  appConf <- ask
  ks <- parseApp (filePath appConf)
  liftIO $ print (appMode appConf)
  mapM_ (routeApp (appMode appConf)) ks
   where routeApp :: Maybe AppMode -> KeepalivedConf -> App ()
         routeApp (Just (SearchApp (SearchVIP i)))  ks = (searchVipApp  i ks >>= liftIO . mapM_ print)
         routeApp (Just (SearchApp (SearchRIP i)))  ks = (searchRipApp  i ks >>= liftIO . mapM_ print)
         routeApp (Just (SearchApp (SearchVRID i))) ks = (searchVridApp i ks >>= liftIO . mapM_ print)
         routeApp _                                 _  = return ()

searchVridApp :: Vrid -> KeepalivedConf -> App [VrrpInstance]
searchVridApp i (KeepalivedConf kct) = return $ foldr search [] kct
  where search :: KeepalivedConfType -> [VrrpInstance] -> [VrrpInstance]
        search (TVrrpInstance vi) vis
          | vrid vi == i              = vi:vis
        search _                  vis = vis

searchVipApp  :: IPAddr -> KeepalivedConf -> App [VirtualServer]
searchVipApp i (KeepalivedConf kct) = return $ foldr search [] kct
  where search :: KeepalivedConfType -> [VirtualServer] -> [VirtualServer]
        search (TVirtualServer vs) vis =
          case virtualServerId vs of
            VirtualServerIpId (RealServerAddress i' _) -> if i == i' then vs:vis
                                                                     else vis
            _                                          -> vis
        search _                   vis = vis

searchRipApp  :: IPAddr -> KeepalivedConf -> App [RealServer]
searchRipApp  = undefined

-- | Configuration for the application
data AppConfig = AppConfig { filePath   :: [FilePath]
                           , verbosity  :: Verbosity
                           , appMode    :: Maybe AppMode
                           }

data AppMode = SearchApp SearchMode
             | VerifyApp VerifyMode
             deriving Show

data SearchMode = SearchVIP  IPAddr
                | SearchRIP  IPAddr
                | SearchIP   IPAddr
                | SearchVRID Vrid
                deriving Show

data VerifyMode = VerifyVRRP
                | VerifyLVS
                | VerifyAll
                deriving Show

-- | Default Configurations
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig { filePath  = ["/etc/keepalived/keepalived.conf"]
                             , verbosity = Quiet
                             , appMode   = Nothing
                             }

-- | Verbose outputs
verboseAppConfig :: AppConfig
verboseAppConfig = defaultAppConfig { verbosity = Verbose }

-- | Debug outputs
debugAppConfig :: AppConfig
debugAppConfig = defaultAppConfig { verbosity = Debug }

-- | Verbosity of outputs
data Verbosity = Quiet | Verbose | Debug deriving (Show, Eq, Ord, Bounded)

-- * Command
data Command = Command
  { commandName  :: String
  , commandAlias :: String
  , commandApp   :: App ()
  , commandOpts  :: [OptDescr Option]
  }

allCommands :: [Command]
allCommands = [ helpCommand, dumpCommand, verifyCommand, searchCommand ]

helpCommand :: Command
helpCommand = Command
  { commandName  = "help"
  , commandAlias = "h"
  , commandApp   = helpApp
  , commandOpts  = []
  }

dumpCommand :: Command
dumpCommand = Command
  { commandName  = "dump"
  , commandAlias = "d"
  , commandApp   = dumpApp
  , commandOpts  = []
  }

verifyCommand :: Command
verifyCommand = Command
  { commandName  = "verify"
  , commandAlias = "v"
  , commandApp   = verifyApp
  , commandOpts  = verifyOpts
  }

searchCommand :: Command
searchCommand = Command
  { commandName  = "search"
  , commandAlias = "s"
  , commandApp   = searchApp
  , commandOpts  = searchOpts
  }

-- | Option
data Option = OptVerbose
            | OptHelp
            | OptVersion
            | OptConf FilePath
            | OptVerifyVrrp
            | OptVerifyLvs
            | OptVerifyAll
            | OptSearchRip String
            | OptSearchVip String
            | OptSearchVrid String
            deriving (Show, Eq)

commonOpts :: [OptDescr Option]
commonOpts = [ Option ['h', '?'] ["help"]    (NoArg OptHelp)         "Show this message"
             , Option ['v']      ["verbose"] (NoArg OptVerbose)      "Verbose outputs"
             , Option ['V']      ["version"] (NoArg OptVersion)      "Show version info"
             , Option ['f']      ["conf"]    (ReqArg OptConf "CONF") "Use specified configuration"
             ]

verifyOpts :: [OptDescr Option]
verifyOpts = [ Option []         ["vrrp"]    (NoArg OptVerifyVrrp)   "Verify VRRP configurations"
             , Option []         ["lvs"]     (NoArg OptVerifyLvs)    "Verify LVS configurations"
             , Option ['a']      ["all"]     (NoArg OptVerifyAll)    "Verify VRRP and LVS configurations"
             ] ++ commonOpts


searchOpts :: [OptDescr Option]
searchOpts = [ Option []         ["rip"]     (ReqArg OptSearchRip "RIP") "Search RIP"
             , Option []         ["vip"]     (ReqArg OptSearchVip "VIP") "Search VIP"
             , Option []         ["vrid"]    (ReqArg OptSearchVrid "VRID") "Search VRID"
             ] ++ commonOpts

optToAppMode :: Option -> Maybe AppMode
optToAppMode OptVerbose        = Nothing
optToAppMode OptVersion        = Nothing
optToAppMode OptHelp           = Nothing
optToAppMode (OptConf _)       = Nothing
optToAppMode OptVerifyVrrp     = Just $ VerifyApp VerifyVRRP
optToAppMode OptVerifyLvs      = Just $ VerifyApp VerifyLVS
optToAppMode OptVerifyAll      = Just $ VerifyApp VerifyAll
optToAppMode (OptSearchRip s)  = SearchApp . SearchRIP <$> ipAddr s
optToAppMode (OptSearchVip s)  = SearchApp . SearchVIP <$> ipAddr s
optToAppMode (OptSearchVrid i) = Just $ SearchApp $ SearchVRID $ Vrid $ read i