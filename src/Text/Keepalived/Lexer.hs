{-# LANGUAGE FlexibleContexts #-}
module Text.Keepalived.Lexer
  ( -- * Top-level lexers
    runLexer
  , token
  , tokens
    -- * Types
  , LexerContext (..)
  , LexerState
  , Token
  , TokenType (..)
    -- * Lexers
  , tIdentifier
  , tBlockId
  , tQuoted
  , tValue
  , tBrace
  , tIncluded
    -- * Utilities
  , lexeme
  , symbol
  , whiteSpace
  ) where

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

-- | Lexer driver
runLexer :: Stream s m t => ParsecT s LexerState m a -> SourceName -> s -> m (Either ParseError a)
runLexer lexer = runParserT lexer []

-- | Lexer context contains current working directory, current parsing position and
--   current inputs.
data LexerContext = LexerContext
  { curDir :: FilePath   -- ^ Current working directory
  , curPos :: SourcePos  -- ^ Parsing position
  , curInp :: String     -- ^ Inputs to parse
  }

-- | History of lexer contexts
type LexerState = [LexerContext]

type Token = (SourcePos, TokenType)
data TokenType = Identifier String             -- ^ Normal identifier
               | BlockId String                -- ^ Blocked identifier
               | OpenBrace                     -- ^ Open brace '{'
               | CloseBrace                    -- ^ Close brace '}'
               | Included [Token]              -- ^ \'include\' identifier and included tokens
               | Quoted { unQuoted :: String } -- ^ \"quoted string\"
               | Value  { unValue  :: String } -- ^ Other value
               deriving (Show, Eq)

-- | Parses a @Token@. Returns the parsed token.
token :: Stream String IO Char => ParsecT String LexerState IO Token
token = lexeme $ (,) <$> getPosition
                     <*> choice [ try tIdentifier
                                , try tBlockId
                                , tBrace
                                , try tIncluded
                                , tQuoted
                                , tValue
                                ]

-- | Parses many @Token@s. Returns the parsed tokens.
tokens :: Stream String IO Char => ParsecT String LexerState IO [Token]
tokens = foldr expand [] <$> (whiteSpace >> many token)
  where expand (_, Included ts) ts' = ts ++ ts'
        expand ts               ts' = ts:ts'

-- | Parses an identifier.
tIdentifier :: Stream s m Char => ParsecT s u m TokenType
tIdentifier = lexeme $ Identifier <$> buildChoices identifiers

-- | Parses a blocked identifier.
tBlockId :: Stream s m Char => ParsecT s u m TokenType
tBlockId = lexeme $ BlockId <$> buildChoices blockIdentifiers

-- | Parses '{' or '}'. Returns @OpenBlace@ or @CloseBrace@.
tBrace :: Stream s m Char => ParsecT s u m TokenType
tBrace = lexeme $ choice [ char '{' >> return OpenBrace
                         , char '}' >> return CloseBrace ]

-- | Parses a quoted string.
tQuoted :: Stream s m Char => ParsecT s u m TokenType
tQuoted = lexeme $ Quoted <$> between (char '"') (char '"') (many quotedChar)
  where quotedChar = try escapedQ <|> satisfy (/= '"')
        escapedQ   = string "\\\"" >> return '"'

-- | Parses an other value.
tValue :: Stream s m Char => ParsecT s u m TokenType
tValue = lexeme $ Value <$> many1 (satisfy $ not . isSpace)

-- | Parses \'include\' directives. Includes specified files, and parses recursively.
tIncluded :: Stream String IO Char => ParsecT String LexerState IO TokenType
tIncluded = lexeme $ do
  symbol "include"
  files <- getGlob
  saveLexerContext
  toks <- mapM lexFile files
  restoreLexerContext
  return $ Included $ concat toks

lexFile :: Stream String IO Char => FilePath -> ParsecT String LexerState IO [Token]
lexFile file = do
  setPosition $ initialPos file
  contents <- liftIO $ readFile file
  setInput contents
  tokens

getGlob :: Stream s IO Char => ParsecT s u IO [FilePath]
getGlob = do
  glob <- many1 $ satisfy (not . isSpace)
  srcDir <- takeDirectory . sourceName <$> getPosition
  liftIO $ do
    setCurrentDirectory srcDir
    canonicalizeGlob glob
  where canonicalizeGlob = namesMatching >=> mapM canonicalizePath

saveLexerContext :: Stream String IO Char => ParsecT String LexerState IO ()
saveLexerContext = do
  states <- getState
  cwd <- liftIO getCurrentDirectory
  pos <- getPosition
  inp <- getInput
  putState $ LexerContext cwd pos inp:states

restoreLexerContext :: Stream String IO Char => ParsecT String LexerState IO ()
restoreLexerContext = do
  (LexerContext cwd pos inp:ss) <- getState
  liftIO $ setCurrentDirectory cwd
  setPosition pos
  setInput inp
  setState ss

-- | @lexeme p@ parses @p@, and strips following white spaces.
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* whiteSpace

-- | @symbol s@ parses @s@. Following characters must be white spaces.
symbol :: Stream s m Char => String -> ParsecT s u m String
symbol s = lexeme $ string s <* notFollowedBy (satisfy $ not . isSpace)

-- | Parses white spaces or comments.
whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = skipMany $ simpleSpace <|> oneLineComment
  where simpleSpace :: Stream s m Char => ParsecT s u m ()
        simpleSpace = skipMany1 $ satisfy isSpace 
        oneLineComment :: Stream s m Char => ParsecT s u m ()
        oneLineComment = try (oneOf "#!") >> skipMany (satisfy (/= '\n'))

-- internal use
-- naiveBuildChoices :: Stream s m Char => [String] -> ParsecT s u m String
-- naiveBuildChoices ss = choice $ try . string <$> reverse (sort ss)

-- | Build left-factored lexer
buildChoices :: Stream s m Char => [String] -> ParsecT s u m String
buildChoices = choice . map (toParser []) . foldr P.insert []
  where eow = skipMany1 (satisfy isSpace) <|> eof
        toParser pfx (P.Bin p ps) = string p >> choice (map (toParser (pfx ++ p)) ps)
        toParser pfx (P.Tip s)    = (pfx ++) <$> string s <* eow
        toParser pfx P.Nil        = eow >> return pfx

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
