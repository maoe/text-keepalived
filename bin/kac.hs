-- {-# LANGUAGE ScopedTypeVariables #-}
import Text.Keepalived.Parser
import Text.Keepalived.Lexer
import Data.Either
import Text.Parsec hiding (tokens)
import System.Environment
import System.Console.GetOpt
import Control.Monad
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  (flags, files) <- compilerOpts args
  case (flags, files) of
    ([], _) -> mapM_ (parseFromFile pKeepalivedConf) files
                 `catch` \e -> do { print e; exitFailure }
    (_,  _) -> mapM_ (parseFromFile pKeepalivedConf >=> print) files
                 `catch` \e -> do { print e; exitFailure }

parseFromFile :: Show a => Parsec [Token] () a -> FilePath -> IO a
parseFromFile p f = do
  input <- readFile f
  toks <- runLexer tokens f input
  case toks of
    Right toks' -> do
      case runParser p () "" toks' of
        Right x -> return x
        Left err -> error $ show err
    Left err    -> error $ show err

data Flag = Verbose

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "verbose" ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, e)  -> ioError $ userError $ concat e ++ usageInfo header options
  where header = "usage kac [OPTION...] files..."

