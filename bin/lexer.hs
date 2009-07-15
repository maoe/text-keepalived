import Text.Keepalived.Lexer
import Data.Either
import System.Environment

lexFromFile :: FilePath -> IO ()
lexFromFile f = do
  input <- readFile f
  toks <- runLexer tokens f input
  case toks of
    Right toks' -> mapM_ print toks'
    Left err    -> print err

main :: IO ()
main = do
  fs <- getArgs
  mapM_ (\f -> lexFromFile f >>= print) fs
