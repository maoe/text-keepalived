{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kc.Option where
import Data.Typeable
import Data.Monoid
import Data.Function
import Control.Applicative
import Control.Monad.Reader
import System.Environment
import System.Exit

import Text.Keepalived
import Text.Keepalived.Types

-- * Application
-- | Application type
newtype App a = App { runA :: ReaderT AppConfig IO a }
              deriving (Monad, MonadIO, MonadReader AppConfig)

runApp :: App a -> AppConfig -> IO a
runApp = runReaderT . runA

-- | Application constructor
{-
getApp :: Command -> App ()
getApp HelpCommand    = helpApp
getApp VersionCommand = versionApp
getApp DumpCommand    = dumpApp
getApp VerifyCommand  = verifyApp
getApp SearchCommand  = searchApp
-}

-- | Print help messages
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
    Verbose -> verifyVerbosely (filePath appConf)
    Debug   -> verifyVerbosely (filePath appConf)
    _       -> verify          (filePath appConf)
  where verify          f = parseApp f                          >> printOK
        verifyVerbosely f = parseApp f >>= liftIO . mapM_ print >> printOK
        printOK = liftIO $ putStrLn "OK"

-- | Dump parsed keepalived.conf
dumpApp :: App ()
dumpApp = do
  conf <- ask
  ks <- parseApp (filePath conf)
  liftIO $ mapM_ print ks

-- | Search specified VIP, RIP or VRID from keepalived.conf
searchApp :: App ()
searchApp = do
  conf <- ask
  ks <- parseApp (filePath conf)
  liftIO $ print ks

-- | Configuration for the application
data AppConfig = AppConfig { filePath   :: [FilePath]
                           , verbosity  :: Verbosity
                           } deriving Show

-- | Default Configurations
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig { filePath  = ["/etc/keepalived/keepalived.conf"]
                             , verbosity = Quiet
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
{-
class Command c where
  name :: c -> String

data GlobalCommand = Verify
                   | Dump
                   deriving Show

instance Command GlobalCommand where
  name Verify = "verify"
  name Dump   = "dump"

data SubCommand = Verbose


main' :: IO ()
main' = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = runApp (getApp command subCommand) config
  where config     = defaultAppConfig
        command    = Verify
        subCommand = Just Verbose


verifyVerboseApp :: App ()
verifyVerboseApp = App $ do
  path <- defaultPath <$> ask
  liftIO $ mapM_ (parseFromFile >=> print) path
                 `catch` \e -> do { print e; exitFailure }

test = runApp verifyApp defaultAppConfig
-}

{-
data SubCommand s => Command s f = Command
  { commandName         :: String
  , commandAlias        :: String
  , subCommand          :: s
  , commandDefaultFlags :: f
  }

class SubCommand c where
  subCommandName :: c -> String

data Flag a = Flag a | NoFlag deriving Show

instance Monoid (Flag a) where
  mempty = NoFlag
  _ `mappend` f@(Flag _) = f
  f `mappend` NoFlag     = f

data VerifyCommand = VerifyCommand

instance Command VerifyCommand VerifyFlags where
  commandName         = "verify"
  commandAlias        = "v"
  subCommand          = defaultVerifySubCommand
  commandDefaultFlags = defaultVerifyVerbose

data VerifySubCommand = VerifyLVS
                      | VerifyVRRP

type VerifyVerbose = Flag Bool
defaultVerifyVerbose :: VerifyVerbose
defaultVerifyVerbose = Flag False
-}
