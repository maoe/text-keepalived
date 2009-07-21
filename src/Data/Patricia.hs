{-# LANGUAGE PatternGuards #-}
module Data.Patricia where
import Control.Monad
import qualified Data.List as L (null)

type PatriciaForest a = [Patricia a]

data Patricia a = Nil
                | Tip a
                | Bin a (PatriciaForest a)
                deriving Show

instance Functor Patricia where
  fmap _ Nil         = Nil
  fmap f (Tip x)     = Tip (f x)
  fmap f (Bin x pfs) = Bin (f x) (map (fmap f) pfs)

null :: PatriciaForest a -> Bool
null [] = True
null _  = False

size :: PatriciaForest a -> Int
size = sum . map size'
  where size' (Bin _ ps) = sum (map size' ps)
        size' (Tip _)    = 1
        size' Nil        = 1

empty :: PatriciaForest a
empty = []

singleton :: a -> PatriciaForest a
singleton = (:[]) . Tip

insert :: Eq a => [a] -> PatriciaForest [a] -> PatriciaForest [a]
insert [] ps  = ps
insert x  []  = singleton x
insert xxs@(x:_) (p:ps)
  | Just h <- takeHead p
  , x == h    = insert' xxs p:ps
  | otherwise = p:insert xxs ps
  where takeHead :: Patricia [a] -> Maybe a
        takeHead Nil           = Nothing
        takeHead (Tip [])      = Nothing
        takeHead (Tip (x:_))   = Just x
        takeHead (Bin [] _)    = Nothing
        takeHead (Bin (x:_) _) = Just x
  
insert' :: Eq a => [a] -> Patricia [a] -> Patricia [a]
insert' x Nil        = Tip x
insert' x (Tip y)    = case x `prefix` y of
                         Equal e          -> Tip e
                         Prefix a (L l)   -> Bin a [Nil, Tip l]
                         Prefix a (R r)   -> Bin a [Nil, Tip r]
                         Prefix a (D l r) -> Bin a [Tip l, Tip r]
                         _                -> error ""
insert' x (Bin y ps) = case x `prefix` y of
                         Equal _          -> Bin x (Nil:ps)
                         Prefix a (L l)   -> Bin a (insert l ps)
                         Prefix a (R r)   -> Bin a [Nil, Bin r ps]
                         Prefix a (D l r) -> Bin a [Tip l, Bin r ps]
                         _                -> error ""

data Prefix a = Equal a
              | Different a a
              | Prefix a (Rest a)
              deriving Show

data Rest a = L a | R a | D a a deriving Show


sample :: PatriciaForest String
sample = [ Bin "a" [ Bin "oe" [ Nil
                              , Bin "mitsu" [ Tip "koshi"
                                            , Tip "toshi" ] ]
                   , Tip "ho" ]
         , Tip "o" ]

{-
end = whiteSpace <|> eof
lexer = choice [ string "a" >> choice [ string "oe" >> choice [ end
                                                              , string "mitsu" >> choice [ string "koshi" >> end
                                                                                         , string "toshi" >> end ] ]
                                      , string "ho" >> end ]
               , string "ho" >> end ]
-}

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

prefix :: Eq a => [a] -> [a] -> Prefix [a]
prefix [] [] = Equal []
prefix xs [] = Prefix [] (L xs)
prefix [] ys = Prefix [] (R ys)
prefix xxs@(x:xs) yys@(y:ys)
  | xxs == yys = Equal xxs
  | x   == y   = Prefix (x:common xs ys) (rest xs ys)
  | x   /= y   = Different xxs yys
  where common = (foldr go [] .) . zip
          where go (x, y) a | x == y    = x:a
                            | otherwise = []
        rest xs [] = L xs
        rest [] ys = R ys
        rest xxs@(x:xs) yys@(y:ys)
          | x == y    = rest xs ys
          | otherwise = D xxs yys
