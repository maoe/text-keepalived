{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Main where
import Control.Applicative hiding (empty)
import Control.Monad.Reader
-- import Data.Generics.PlateData
import System.Console.CmdArgs
import System.Environment
import System.IO
import Text.Keepalived

main :: IO ()
main = do
  n <- getProgName
  m <- cmdArgs (n ++ " version 0.0.1") [verify, dump]
  v <- verbosity
  runApp mainApp (AppConf m v)

-- * Application types
newtype App a = App { runA :: ReaderT AppConf IO a }
              deriving (Functor, Monad, MonadIO, MonadReader AppConf)

data AppConf = AppConf { appMode :: KcMode
                       , appVerb :: Verbosity
                       } deriving Show

runApp :: App a -> AppConf -> IO a
runApp a = runReaderT (runA a) . fillInFiles
  where fillInFiles (AppConf m v)
          | files m == [] = AppConf (m { files = ["/etc/keepalived/keepalived.conf"] }) v
          | otherwise     = AppConf m                                                   v

-- * App implementations
mainApp :: App ()
mainApp = do
  m <- asks appMode
  case m of
    Verify _   -> verifyApp
    Dump   _   -> dumpApp
    -- Search _ _ -> searchApp
    -- List   _ _ -> listApp

verifyApp :: App ()
verifyApp = do
  m <- asks appMode
  parseApp (files m)
  msg (hPutStr stderr "lexical/syntactic verification: ") >> okApp

dumpApp :: App ()
dumpApp = do
  AppConf m _ <- local (\c -> c { appVerb = Verbose }) ask
  parseApp (files m) >>= msg . mapM_ print

{- TODO
searchApp :: App ()
searchApp = do
  m <- asks appMode
  cs <- parseApp (files m)
  case target m of
    VRID _ -> const noopApp cs
    VIP  _ -> const noopApp cs
    _      -> const noopApp cs

listApp :: App ()
listApp = do
  m <- asks appMode
  cs <- parseApp (files m)
  case target m of
    VRID _ -> const noopApp cs
    VIP  _ -> const noopApp cs
    _      -> const noopApp cs

listVRID :: Data a => a -> [(Vrid, [Ipaddress])]
listVRID x = [ (vrid vi, virtualIpaddress vi ++ virtualIpaddressExcluded vi)
             | TVrrpInstance vi <- universeBi x ]

listVIP :: Data a => a -> [Ipaddress]
listVIP x = concat [ virtualIpaddress vi ++ virtualIpaddressExcluded vi
                   | TVrrpInstance vi <- universeBi x ]
-}

noopApp :: App ()
noopApp = return ()

-- * Util Apps
parseApp :: [FilePath] -> App [KeepalivedConf]
parseApp fs = do
  c <- forM fs $ \f -> do
    msg $ hPutStrLn stderr $ "parsing " ++ f
    liftIO $ parseFromFile f
  verbMsg $ mapM_ print c
  return c

msg :: IO a -> App ()
msg io = do
  v <- asks appVerb
  case v of
    Quiet -> return ()
    _     -> liftIO io >> return ()

verbMsg :: IO a -> App ()
verbMsg io = do
  v <- asks appVerb
  case v of
    Verbose -> liftIO io >> return ()
    _       -> return ()

okApp :: App ()
okApp = msg $ hPutStrLn stderr "OK"

-- * Command-line parameters
data KcMode = Verify { files :: [FilePath] }
            | Dump   { files :: [FilePath] }
            -- | Search { files :: [FilePath], target :: Target }
            -- | List   { files :: [FilePath], target :: Target }
            deriving (Data, Typeable, Show, Eq)

data Target = VRID Int
            | VIP  String
            | RIP  String
            deriving (Data, Typeable, Show, Eq)

verify :: Mode KcMode
verify = mode $ Verify
  { files = def &= args
  } &= text "Verify configuration files." & defMode

dump :: Mode KcMode
dump = mode $ Dump
  { files = def &= args
  } &= text "Dump configuration files."

{- TODO
search :: Mode KcMode
search = mode $ Search
  { target = enum (VRID def)
                  [ VRID def           &= text "Search VRID (0-255)"
                  , VIP  "192.168.0.1" &= text "Search virtual IP addresses"
                  , RIP  "192.168.0.1" &= text "Search real IP addresses" ]
  , files = defConf &= typFile & args
  } &= text "Search (VRID|VIP|RIP) from configuration files."

list :: Mode KcMode
list = mode $ List
  { target = enum (VRID def)
                  [ VRID def           &= text "List VRID (0-255)"
                  , VIP  "192.168.0.1" &= text "List virtual IP addresses"
                  , RIP  "192.168.0.1" &= text "List real IP addresses" ]
  , files = defConf &= typFile & args
  } &= text "List (VRID|VIP|RIP) from configuration files."
-}

data Verbosity = Quiet | Normal | Verbose
               deriving (Show, Eq, Ord, Enum)

verbosity :: IO Verbosity
verbosity = do
  norm <- fromEnum <$> isNormal
  loud <- fromEnum <$> isLoud
  return $ toEnum $ norm + loud
