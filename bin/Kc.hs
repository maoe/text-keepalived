module Main where
import Data.List
import Control.Monad
import System.Environment
import System.Console.GetOpt

import Kc.Option

main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker []             = runApp helpApp defaultAppConfig
mainWorker (cmdName:args) = do
  case command of
    Nothing  -> runApp helpApp defaultAppConfig
    Just cmd -> handleOption cmd args
  where command = find (\c -> commandName  c == cmdName) allCommands `mplus`
                  find (\c -> commandAlias c == cmdName) allCommands

handleOption :: Command -> [String] -> IO ()
handleOption command args = do
  case getOpt Permute (commandOpts command) args of
    (opts, rests, []  ) -> runApp (commandApp command) (processAppConfig opts rests)
    (_,    _,     errs) -> print errs >> runApp helpApp defaultAppConfig

processAppConfig :: [Option] -> [String] -> AppConfig
processAppConfig opts args = AppConfig { filePath  = args
                                       , verbosity = verbosity
                                       , appMode   = appMode }
    where verbosity = maybe Quiet (const Verbose) (find (== OptVerbose) opts)
          appMode   = msum $ map optToAppMode opts