module Text.Keepalived
  ( -- * Module imports
    module Text.Keepalived.Parser
  , module Text.Keepalived.Lexer
  , module Text.Keepalived.Types
    -- * Helper functions
  , parseFromFile
  , parseString
  ) where

import Text.Keepalived.Parser
import Text.Keepalived.Lexer
import Text.Keepalived.Types
import Data.Either
import Text.Parsec hiding (tokens)

parseFromFile :: FilePath -> IO KeepalivedConf
parseFromFile f = do
  input <- readFile f
  toks <- runLexer tokens f input
  case toks of
    Right toks' -> do
      case runParser pKeepalivedConf () "" toks' of
        Right x -> return x
        Left err -> error $ show err
    Left err    -> error $ show err

parseString :: String -> IO KeepalivedConf
parseString s = do
  toks <- runLexer tokens "" s
  case toks of
    Right toks' -> do
      case runParser pKeepalivedConf () "" toks' of
        Right x -> return x
        Left err -> error $ show err
    Left err    -> error $ show err
