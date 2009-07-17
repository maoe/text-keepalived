-- {-# LANGUAGE ScopedTypeVariables #-}
import Text.Keepalived
import System.Environment
import System.Console.GetOpt
import Control.Monad
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  (flags, files) <- compilerOpts args
  case (flags, files) of
    ([], _) -> mapM_ parseFromFile files
                 `catch` \e -> do { print e; exitFailure }
    (_,  _) -> mapM_ (parseFromFile >=> print) files
                 `catch` \e -> do { print e; exitFailure }

data Flag = Verbose

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "verbose" ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, e)  -> ioError $ userError $ concat e ++ usageInfo header options
  where header = "usage kac [OPTION...] files..."

