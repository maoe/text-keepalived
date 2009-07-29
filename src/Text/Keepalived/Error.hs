{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables #-}
module Text.Keepalived.Error
  ( -- * Lexical errors
    LexicalError(..)
  , NoSuchFile(..)
    -- * Syntactic errors
  , SyntacticError(..)
    -- * Semantic errors
  , SemanticError(..)
  ) where

import Control.Exception
import Data.Typeable

-- lexical errors
data LexicalError = forall e. Exception e => LexicalError e deriving Typeable

instance Show LexicalError where
  show (LexicalError e) = show e

instance Exception LexicalError

lexerErrorToException :: Exception e => e -> SomeException
lexerErrorToException = toException . LexicalError

lexerErrorFromException :: Exception e => SomeException -> Maybe e
lexerErrorFromException x = do
  LexicalError e <- fromException x
  cast e

data NoSuchFile = NoSuchFile FilePath deriving Typeable

instance Exception NoSuchFile where
  toException   = lexerErrorToException
  fromException = lexerErrorFromException

instance Show NoSuchFile where
  show (NoSuchFile p) = "No such file: " ++ p

-- syntactic errors
data SyntacticError = forall e. Exception e => SyntacticError e deriving Typeable

instance Show SyntacticError where
  show (SyntacticError e) = show e

instance Exception SyntacticError

{-
syntacticErrorToException :: Exception e => e -> SomeException
syntacticErrorToException = toException . SyntacticError

syntacticErrorFromException :: Exception e => SomeException -> Maybe e
syntacticErrorFromException x = do
  SyntacticError e <- fromException x
  cast e
-}

-- semantic errors
data SemanticError = forall e. Exception e => SemanticError e deriving Typeable

instance Show SemanticError where
  show (SemanticError e) = show e

instance Exception SemanticError

{-
semanticErrorToException :: Exception e => e -> SomeException
semanticErrorToException = toException . SemanticError

semanticErrorFromException :: Exception e => SomeException -> Maybe e
semanticErrorFromException x = do
  SemanticError e <- fromException x
  cast e
-}
