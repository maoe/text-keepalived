{-# LANGUAGE PatternGuards #-}
module Data.Patricia
  ( Patricia(..)
  , empty
  , singleton
  , null
  , size
  , insert
  ) where

import Prelude hiding (null)

type PatriciaForest a = [Patricia a]

data Patricia a = Nil
                | Tip a
                | Bin a (PatriciaForest a)
                deriving Show

instance Functor Patricia where
  fmap f (Bin x pfs) = Bin (f x) (map (fmap f) pfs)
  fmap f (Tip x)     = Tip (f x)
  fmap _ Nil         = Nil

-- | Create an empty tree
empty :: PatriciaForest a
empty = []

-- | Create an single value tree
singleton :: a -> PatriciaForest a
singleton = (:[]) . Tip

null :: PatriciaForest a -> Bool
null [] = True
null _  = False

size :: PatriciaForest a -> Int
size = sum . map treeSize
  where treeSize (Bin _ ps) = sum $ map treeSize ps
        treeSize (Tip _)    = 1
        treeSize Nil        = 1



insert :: Eq a => [a] -> PatriciaForest [a] -> PatriciaForest [a]
insert [] ps  = ps
insert x  []  = singleton x
insert xxs@(x:_) (p:ps)
  | Just h <- takeHead p
  , x == h    = treeInsert xxs p:ps
  | otherwise = p:insert xxs ps
  where takeHead :: Patricia [a] -> Maybe a
        takeHead (Tip (x:_))   = Just x
        takeHead (Bin (x:_) _) = Just x
        takeHead Nil           = Nothing
        takeHead (Tip [])      = Nothing
        takeHead (Bin [] _)    = Nothing

        treeInsert :: Eq a => [a] -> Patricia [a] -> Patricia [a]
        treeInsert x Nil        = Tip x
        treeInsert x (Tip y)    = case x `prefix` y of
          Prefix a (L l)   -> Bin a [Nil, Tip l]
          Prefix a (R r)   -> Bin a [Nil, Tip r]
          Prefix a (D l r) -> Bin a [Tip l, Tip r]
          Equal e          -> Tip e
          _                -> error ""
        treeInsert x (Bin y ps) = case x `prefix` y of
          Prefix a (L l)   -> Bin a (insert l ps)
          Prefix a (R r)   -> Bin a [Nil, Bin r ps]
          Prefix a (D l r) -> Bin a [Tip l, Bin r ps]
          Equal _          -> Bin x (Nil:ps)
          _                -> error ""

data Prefix a = Equal a
              | Different a a
              | Prefix a (Rest a)
              deriving Show

data Rest a = L a | R a | D a a deriving Show

{-
naivePrefix :: Eq a => [a] -> [a] -> Prefix [a]
naivePrefix xs ys = go xs ys []
  where go :: Eq a => [a] -> [a] -> [a] -> Prefix [a]
        go [] [] rs   = Equal (reverse rs)
        go xs [] rs   = Prefix (reverse rs) (L xs)
        go [] ys rs   = Prefix (reverse rs) (R ys)
        go xxs@(x:xs) yys@(y:ys) rs
          | x == y    = go xs ys (x:rs)
          | L.null rs = Different xxs yys
          | otherwise = Prefix (reverse rs) (D xxs yys)
-}

prefix :: Eq a => [a] -> [a] -> Prefix [a]
prefix xxs@(x:xs) yys@(y:ys)
  | xxs == yys = Equal xxs
  | x   == y   = Prefix (x:common xs ys) (rest xs ys)
  | otherwise  = Different xxs yys
  where rest xs [] = L xs
        rest [] ys = R ys
        rest xxs@(x:xs) yys@(y:ys)
          | x == y    = rest xs ys
          | otherwise = D xxs yys
        common = (foldr go [] .) . zip
          where go (x, y) a | x == y    = x:a
                            | otherwise = []
prefix [] [] = Equal []
prefix xs [] = Prefix [] (L xs)
prefix [] ys = Prefix [] (R ys)
