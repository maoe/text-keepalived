{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Control.Monad.Reader
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment
import System.Exit
import System.Console.GetOpt
import Text.PrettyPrint.HughesPJ

import Network.Layer3
import Text.Keepalived

-- Types
newtype App a = App { runA :: ReaderT AppConfig IO a }
                deriving (Functor, Monad, MonadIO, MonadReader AppConfig)

runApp :: App a -> AppConfig -> IO a
runApp = runReaderT . runA

data AppConfig = AppConfig { filePath  :: [FilePath]
                           , verbosity :: Verbosity
                           , appOpts   :: [AppOpt]
                           }

data Verbosity = Quiet | Verbose | Debug deriving (Show, Eq, Ord, Bounded)

-- Apps
helpApp :: App ()
helpApp = liftIO $ do
            name <- getProgName
            putStrLn $ render $ helpMessage name
  where helpMessage n =
          vcat [ text n <+> text "verify [keepalived.conf]"
               , text n <+> text "search (--vip|--rip|--vrid) [keepalived.conf]"
               , text n <+> text "list [keepalived.conf]"
               , text n <+> text "dump [keepalived.conf]"
               ]

dumpApp :: App ()
dumpApp = do
  AppConfig files _ _ <- local verbose ask
  parseApp files >>= mapPrintApp

verbose :: AppConfig -> AppConfig
verbose c = c { verbosity = Verbose }

verifyApp :: App ()
verifyApp = do
  AppConfig files verbosity opts <- ask
  parseApp files
  okApp

listApp :: App ()
listApp = do
  AppConfig files _ opts <- ask
  parsed <- parseApp files
--  listVipApp parsed
  listVridApp parsed

listVridApp :: [KeepalivedConf] -> App ()
listVridApp parsed = liftIO $ do
  mapM_ print $ vcat . fmap (uncurry prettify) . listVrid <$> parsed
  where
    header :: Vrid -> Doc
    header vrid = integer (fromIntegral $ unVrid vrid) <> char ':'
    body :: [Ipaddress] -> Doc
    body addrs  = vcat $ map (text . show) addrs
    prettify :: Vrid -> [Ipaddress] -> Doc
    prettify vrid addrs = header vrid $+$ nest 5 (body addrs)

listVipApp :: [KeepalivedConf] -> App ()
listVipApp parsed = liftIO $ do
  mapM_ print $ vcat . fmap prettify . listVip <$> parsed
  where prettify = text . show
  
type VridMap = Map Vrid [Ipaddress]

listVrid :: Data a => a -> [(Vrid, [Ipaddress])]
listVrid x = [ (vrid vi, virtualIpaddress vi ++ virtualIpaddressExcluded vi)
             | TVrrpInstance vi <- universeBi x ]

listVip :: Data a => a -> [Ipaddress]
listVip x = concat [ virtualIpaddress vi ++ virtualIpaddressExcluded vi
                   | TVrrpInstance vi <- universeBi x ]


searchApp :: App ()
searchApp = do
  AppConfig files verbosity opts <- ask
  parseApp files
  return ()

-- common Apps
parseApp :: [FilePath] -> App [KeepalivedConf]
parseApp files = do
  v <- asks verbosity
  liftIO (mapM parseFromFile files) >>= verboseOutput v
  where verboseOutput v p
          | v >= Verbose = sequence_ (liftM printApp p) >> return p
          | otherwise    = return p

okApp :: App ()
okApp = liftIO $ putStrLn "OK"

printApp :: Show a => a -> App ()
printApp = liftIO . print

mapPrintApp :: Show a => [a] -> App ()
mapPrintApp = sequence_ . fmap printApp

-- AppConfig
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig { filePath  = ["/etc/keepalived/keepalived.conf"]
                             , verbosity = Quiet
                             , appOpts   = []
                             }

verboseAppConfig :: AppConfig
verboseAppConfig = defaultAppConfig { verbosity = Verbose }

debugAppConfig :: AppConfig
debugAppConfig = defaultAppConfig { verbosity = Debug }

-- Commands and options
data Command = Command
  { commandName :: String
  , commandOpts :: [OptDescr AppOpt]
  , commandApp  :: App ()
  }

data AppOpt = OptSearchVip  String
            | OptSearchRip  String
            | OptSearchVrid String
            | OptListVip    String
            | OptListRip    String
            | OptListVrid   String
            | OptHelp
            | OptVerbose
            deriving Eq

searchCommand = Command
  { commandName = "search"
  , commandOpts = [ Option []  ["vip"]  (ReqArg OptSearchVip  "VIP")  "search VIP"
                  , Option []  ["rip"]  (ReqArg OptSearchRip  "RIP")  "search RIP"
                  , Option []  ["vrid"] (ReqArg OptSearchVrid "VRID") "search VRID"
                  , Option "h" ["help"] (NoArg  OptHelp)              "help for search"
                  ]
  , commandApp  = searchApp
  }

listCommand = Command
  { commandName = "list"
  , commandOpts = [ Option []  ["vip"]     (ReqArg OptListVip  "VRID") "list VIP"
                  , Option []  ["rip"]     (ReqArg OptListRip  "VRID") "list RIP"
                  , Option []  ["vrid"]    (ReqArg OptListVrid "VRID") "list VRID"
                  , Option "h" ["help"]    (NoArg  OptHelp)            "help for list"
                  , Option "v" ["verbose"] (NoArg  OptVerbose)         "verbose output"
                  ]
  , commandApp  = listApp
  }

dumpCommand = Command
  { commandName = "dump"
  , commandOpts = [ Option "h" ["help"] (NoArg OptHelp) "help for dump"
                  ]
  , commandApp  = dumpApp
  }

verifyCommand = Command
  { commandName = "verify"
  , commandOpts = [ Option "h" ["help"] (NoArg OptHelp) "help for veirfy"
                  ]
  , commandApp  = verifyApp
  }

helpCommand = Command
  { commandName = "help"
  , commandOpts = []
  , commandApp  = helpApp
  }

-- Main
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker (cmd:opts) = runApp (processApp command) (processAppConfig command opts)
  where command = processCommand cmd
mainWorker _          = runApp helpApp defaultAppConfig

processCommand :: String -> Command
processCommand cmd
  | cmd `isPrefixOf` commandName verifyCommand = verifyCommand
  | cmd `isPrefixOf` commandName dumpCommand   = dumpCommand
  | cmd `isPrefixOf` commandName listCommand   = listCommand
  | cmd `isPrefixOf` commandName searchCommand = searchCommand
  | otherwise                                  = helpCommand

processApp :: Command -> App ()
processApp = commandApp

processAppConfig :: Command -> [String] -> AppConfig
processAppConfig cmd opts = do
  case getOpt Permute (commandOpts cmd) opts of
    (options, files, []) -> let verbosity = maybe Quiet (const Verbose) (find (== OptVerbose) options)
                                appOpts   = options
                    in  defaultAppConfig { filePath  = files
                                         , verbosity = verbosity
                                         , appOpts   = appOpts }
    (_, _, errs) -> defaultAppConfig
