{-# LANGUAGE FlexibleContexts #-}
module Text.Keepalived.Lexer where
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Text.Parsec hiding (token, tokens)
import Text.Parsec.Pos

import qualified Data.Patricia as P

-- driver
runLexer :: Stream s m t => ParsecT s () m a -> SourceName -> s -> m (Either ParseError a)
runLexer lexer = runParserT lexer ()

-- types
type Token = (SourcePos, TokenType)
data TokenType = Identifier String
               | BlockId String
               | OpenBrace
               | CloseBrace
               | Included [Token]
               | Quoted { unQuoted :: String }
               | Value  { unValue  :: String }
               deriving (Show, Eq)

-- tokenizers
token :: Stream String IO Char => ParsecT String u IO Token
token = lexeme $ (,) <$> getPosition
                     <*> choice [ try tIdentifier
                                , try tBlockId
                                , tBrace
                                , tIncluded
                                , tQuoted
                                , tValue
                                ]

tokens :: Stream String IO Char => ParsecT String u IO [Token]
tokens = foldr expand [] <$> (whiteSpace >> many token)
  where expand (_, Included ts) ts' = ts ++ ts'
        expand ts               ts' = ts:ts'

tQuoted :: Stream s m Char => ParsecT s u m TokenType
tQuoted = lexeme $ Quoted <$> between (char '"') (char '"') (many quotedChar)
  where quotedChar = try escapedQ <|> satisfy (/= '"')
        escapedQ   = string "\\\"" >> return '"'

tValue :: Stream s m Char => ParsecT s u m TokenType
tValue = lexeme $ Value <$> many1 (satisfy $ not . isSpace)

tBrace :: Stream s m Char => ParsecT s u m TokenType
tBrace = lexeme $ choice [ char '{' >> return OpenBrace
                         , char '}' >> return CloseBrace ]

tIdentifier :: Stream s m Char => ParsecT s u m TokenType
-- tIdentifier = lexeme $ Identifier <$> buildChoices identifiers
tIdentifier = lexeme $ Identifier <$> fastBuildChoices identifiers

tBlockId :: Stream s m Char => ParsecT s u m TokenType
-- tBlockId = lexeme $ BlockId <$> buildChoices blockIdentifiers
tBlockId = lexeme $ BlockId <$> fastBuildChoices blockIdentifiers

tIncluded :: Stream String IO Char => ParsecT String u IO TokenType
tIncluded = lexeme $ do
  try $ symbol "include"
  glob <- many1 $ satisfy $ not . isSpace
  (curPos, curDir, curInp) <- getCtx
  files <- liftIO $ do
    let srcDir = takeDirectory $ sourceName curPos
    setCurrentDirectory srcDir
    fs <- namesMatching glob
    return $ (srcDir </>) <$> fs
  ts <- forM files $ \file -> do
    setPosition $ initialPos file
    contents <- liftIO $ readFile file
    setInput contents
    tokens
  setCtx curPos curDir curInp
  return $ Included $ concat ts
  where getCtx = (,,) <$> getPosition
                      <*> liftIO getCurrentDirectory
                      <*> getInput
        setCtx pos dir inp = do
          setPosition pos
          liftIO $ setCurrentDirectory dir
          setInput inp

-- whitespaces
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* whiteSpace

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol s = lexeme $ string s <* notFollowedBy (satisfy $ not . isSpace)

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = skipMany $ simpleSpace <|> oneLineComment
  where simpleSpace :: Stream s m Char => ParsecT s u m ()
        simpleSpace = skipMany1 $ satisfy isSpace 
        oneLineComment :: Stream s m Char => ParsecT s u m ()
        oneLineComment = try (oneOf "#!") >> skipMany (satisfy (/= '\n'))

-- reserved words
buildChoices :: Stream s m Char => [String] -> ParsecT s u m String
buildChoices ss = choice $ try . string <$> reverse (sort ss)

fastBuildChoices :: Stream s m Char => [String] -> ParsecT s u m String
fastBuildChoices = choice . map (toParser []) . foldr P.insert []
  where eow = skipMany1 (satisfy isSpace) <|> eof
        toParser pfx (P.Bin p ps) = string p >> choice (map (toParser (pfx ++ p)) ps)
        toParser pfx (P.Tip s)    = (pfx ++) <$> string s <* eow
        toParser pfx P.Nil        = eow >> return pfx


ids = [ "ma", "maoe", "maoe_01" ]
identifiers :: [String]
identifiers =
  [ "advert_int"
  , "alpha"
  , "auth_pass"
  , "auth_type"
  , "bindto"
  , "connect_ip"
  , "connect_port"
  , "connect_timeout"
  , "debug"
  , "delay_before_retry"
  , "delay_loop"
  , "digest"
  , "dont_track_primary"
  , "fwmark"
  , "garp_master_delay"
  , "ha_suspend"
  , "helo_name"
  , "hysteresis"
  , "inhibit_on_failure"
  , "interface"
  , "interval"
  , "lb_algo"
  , "lb_kind"
  , "lvs_id"
  , "lvs_method"
  , "lvs_sched"
  , "lvs_sync_daemon_interface"
  , "mcast_src_ip"
  , "misc_dynamic"
  , "misc_path"
  , "misc_timeout"
  , "nat_mask"
  , "nb_get_retry"
  , "nopreempt"
  , "notification_email_from"
  , "notify"
  , "notify_backup"
  , "notify_down"
  , "notify_fault"
  , "notify_master"
  , "notify_up"
  , "omega"
  , "path"
  , "persistence_granularity"
  , "persistence_timeout"
  , "preempt_delay"
  , "priority"
  , "protocol"
  , "quorum"
  , "quorum_down"
  , "quorum_up"
  , "retry"
  , "router_id"
  , "script"
  , "smtp_alert"
  , "smtp_connect_timeout"
  , "smtp_server"
  , "sorry_server"
  , "state"
  , "status_code"
  , "virtual_router_id"
  , "virtualhost"
  , "weight"
  ]

blockIdentifiers :: [String]
blockIdentifiers =
  [ "HTTP_GET"
  , "MISC_CHECK"
  , "SMTP_CHECK"
  , "SSL_GET"
  , "TCP_CHECK"
  , "authentication"
  , "global_defs"
  , "group"
  , "host"
  , "notification_email"
  , "real_server"
  , "static_ipaddress"
  , "static_route"
  , "static_routes"
  , "track_interface"
  , "track_script"
  , "url"
  , "virtual_ipaddress"
  , "virtual_ipaddress_excluded"
  , "virtual_routes"
  , "virtual_server"
  , "virtual_server_group"
  , "vrrp_instance"
  , "vrrp_script"
  , "vrrp_sync_group"
  ]
