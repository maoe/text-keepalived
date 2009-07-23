{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kc.Option where
import Control.Monad.Reader
import Data.Function
import Data.List
import System.Console.GetOpt
import System.Exit

import Text.Keepalived
import Text.Keepalived.Types

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
  appConf <- ask
  case verbosity appConf of
    Verbose -> verifyVerbosely $ filePath appConf
    Debug   -> verifyVerbosely $ filePath appConf
    _       -> verify          $ filePath appConf
  where verify          f = parseApp f                          >> printOK
        verifyVerbosely f = parseApp f >>= liftIO . mapM_ print >> printOK
        printOK = liftIO $ putStrLn "OK"

-- | Dump parsed keepalived.conf
dumpApp :: App ()
dumpApp = do
  path <- asks filePath
  ks <- parseApp path
  liftIO $ mapM_ print ks

-- | Search specified VIP, RIP or VRID from keepalived.conf
searchApp :: App ()
searchApp = do
  path <- asks filePath
  ks <- parseApp path
  liftIO $ print ks

-- | Configuration for the application
data AppConfig = AppConfig { filePath   :: [FilePath]
                           , verbosity  :: Verbosity
                           } deriving Show

-- | Default Configurations
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig { filePath  = ["/etc/keepalived/keepalived.conf"]
                             , verbosity = Quiet }

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
