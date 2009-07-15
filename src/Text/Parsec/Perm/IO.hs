-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Perm
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially quantified data constructors)
-- 
-- This module implements permutation parsers. The algorithm used
-- is fairly complex since we push the type system to its limits :-)
-- The algorithm is described in:
-- 
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
-- Published as a functional pearl at the Haskell Workshop 2001.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

module Text.Parsec.Perm.IO
    ( PermParser
    , StreamPermParser -- abstract

    , permute
    , (<||>), (<$$>)
    , (<|?>), (<$?>)
    ) where

import Text.Parsec

import System.IO

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>


{---------------------------------------------------------------
  test -- parse a permutation of
  * an optional string of 'a's
  * a required 'b'
  * an optional 'c'
---------------------------------------------------------------}
{-
test input
  = parse (do{ x <- ptest; eof; return x }) "" input

ptest :: Parser (String,Char,Char)
ptest
  = permute $
    (,,) <$?> ("",many1 (char 'a'))
         <||> char 'b'
         <|?> ('_',char 'c')
-}

{---------------------------------------------------------------
  Building a permutation parser
---------------------------------------------------------------}

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@. 

(<||>) :: (Stream s IO tok) => StreamPermParser s st (a -> b) -> ParsecT s st IO a -> StreamPermParser s st b
(<||>) perm p     = add perm p

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is not allowed to accept empty input - use the optional
-- combinator ('<$?>') instead.
--
-- If the function @f@ takes more than one parameter, the type variable
-- @b@ is instantiated to a functional type which combines nicely with
-- the adds parser @p@ to the ('<||>') combinator. This
-- results in stylized code where a permutation parser starts with a
-- combining function @f@ followed by the parsers. The function @f@
-- gets its parameters in the order in which the parsers are specified,
-- but actual input can be in any order.

(<$$>) :: (Stream s IO tok) => (a -> b) -> ParsecT s st IO a -> StreamPermParser s st b
(<$$>) f p        = newperm f <||> p

-- | The expression @perm \<||> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@. 

(<|?>) :: (Stream s IO tok) => StreamPermParser s st (a -> b) -> (a, ParsecT s st IO a) -> StreamPermParser s st b
(<|?>) perm (x,p) = addopt perm x p

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead. 

(<$?>) :: (Stream s IO tok) => (a -> b) -> (a, ParsecT s st IO a) -> StreamPermParser s st b
(<$?>) f (x,p)    = newperm f <|?> (x,p)

{---------------------------------------------------------------
  The permutation tree
---------------------------------------------------------------}

-- | Provided for backwards compatibility.  The tok type is ignored.

type PermParser tok st a = StreamPermParser String st a

-- | The type @StreamPermParser s st a@ denotes a permutation parser that,
-- when converted by the 'permute' function, parses 
-- @s@ streams with user state @st@ and returns a value of
-- type @a@ on success.
--
-- Normally, a permutation parser is first build with special operators
-- like ('<||>') and than transformed into a normal parser
-- using 'permute'.

data StreamPermParser s st a = Perm (Maybe a) [StreamBranch s st a]

-- type Branch st a = StreamBranch String st a

data StreamBranch s st a = forall b. Branch (StreamPermParser s st (b -> a)) (ParsecT s st IO b)

-- | The parser @permute perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of:
-- an optional string of @a@'s, the character @b@ and an optional @c@.
-- This can be described by:
--
-- >  test  = permute (tuple <$?> ("",many1 (char 'a'))
-- >                         <||> char 'b' 
-- >                         <|?> ('_',char 'c'))
-- >        where
-- >          tuple a b c  = (a,b,c)

-- transform a permutation tree into a normal parser
permute :: (Stream s IO tok) => StreamPermParser s st a -> ParsecT s st IO a
permute (Perm def xs)
  = choice (map branch xs ++ empty)
  where
    empty
      = case def of
          Nothing -> []
          Just x  -> [return x]

    branch (Branch perm p)
      = do{ x <- p
          ; f <- permute perm
          ; return (f x)
          }

-- build permutation trees
newperm :: (Stream s IO tok) => (a -> b) -> StreamPermParser s st (a -> b)
newperm f
  = Perm (Just f) []

add :: (Stream s IO tok) => StreamPermParser s st (a -> b) -> ParsecT s st IO a -> StreamPermParser s st b
add perm@(Perm _mf fs) p
  = Perm Nothing (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (add (mapPerms flip perm') p) p'

addopt :: (Stream s IO tok) => StreamPermParser s st (a -> b) -> a -> ParsecT s st IO a -> StreamPermParser s st b
addopt perm@(Perm mf fs) x p
  = Perm (fmap ($ x) mf) (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (addopt (mapPerms flip perm') x p) p'


mapPerms :: (Stream s IO tok) => (a -> b) -> StreamPermParser s st a -> StreamPermParser s st b
mapPerms f (Perm x xs)
  = Perm (fmap f x) (map mapBranch xs)
  where
    mapBranch (Branch perm p)
      = Branch (mapPerms (f.) perm) p

