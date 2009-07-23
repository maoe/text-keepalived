module Main where
import Data.List
import System.Environment
import System.Console.GetOpt

import Kc.Option

main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker []             = runApp helpApp defaultAppConfig
mainWorker (cmdName:args) = do
  case getCommand of
    []                        -> runApp helpApp defaultAppConfig
    (cmd@(Command _ _ _ _):_) -> handleOption cmd args
  where getCommand = filter (\c -> commandName  c == cmdName) allCommands
                  ++ filter (\c -> commandAlias c == cmdName) allCommands

handleOption :: Command -> [String] -> IO ()
handleOption command args = do
  case getOpt Permute (commandOpts command) args of
    (opts, rests, []  ) -> runApp (commandApp command) (processAppConfig opts rests)
    (_,    _,     errs) -> print errs >> runApp helpApp defaultAppConfig

processAppConfig :: [Option] -> [String] -> AppConfig
processAppConfig opts args = AppConfig { filePath  = args, verbosity = verbosity }
    where verbosity = maybe Quiet (const Verbose) (find (== OptVerbose) opts)

