-- Fig.9.9: Parsing primitives and combinators
-- Parselib.hs

module Parselib where

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v xs = [(v,xs)]

fail_ :: Parser a b
fail_ xs = []

satisfy :: (a -> Bool) -> Parser a a
satisfy q [] = fail_ []
satisfy q (x:xs) | q x = succeed x xs
		 | otherwise = fail_ xs

literal :: Eq a => a -> Parser a a
literal x = satisfy (==x)

infixr 4 `alt`
alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) xs = p1 xs ++ p2 xs

infixr 6 `seq_`
seq_ :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `seq_` p2) xs =
    [((v,v'),xs'')|(v,xs')<-p1 xs, (v',xs'')<-p2 xs']

infixl 5 `using`
using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) xs = [(f v,xs')|(v,xs')<-p xs]

many :: Parser a b -> Parser a [b]
many p = p `seq_` many p `using` uncurry (:) `alt` succeed []

some :: Parser a b -> Parser a [b]
some p = p `seq_` many p `using` uncurry (:)

infixr 6 `x_seq`, `seq_x`
x_seq :: Parser a b -> Parser a c -> Parser a c
p1 `x_seq` p2 = (p1 `seq_` p2) `using` snd
seq_x :: Parser a b -> Parser a c -> Parser a b
p1 `seq_x` p2 = (p1 `seq_` p2) `using` fst

anyof :: (a -> Parser b c) -> [a] -> Parser b c
anyof p = foldr (alt . p) fail_
