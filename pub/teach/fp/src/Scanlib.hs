-- Fig.9.10: Scanner primitives
-- Scanlib.hs

module Scanlib where
import Parselib

type Token t = (t,[Char])

infix 7 `tok`
tok :: Parser Char [Char] -> t -> Parser Char (Token t)
(p `tok` t) xs = [((t,v),xs')| (v,xs')<-p xs]

lexime :: [(Parser Char [Char], t)] -> Parser Char [Token t]
lexime = many . foldr (alt . uncurry tok) fail_

strip :: Eq t => t -> [Token t] -> [Token t]
strip t = filter ((/= t) . fst)

kind :: Eq t => t -> Parser (Token t) [Char]
kind t = satisfy ((== t) . fst) `using` snd

number :: Parser Char [Char]
number = some (satisfy isDigit)

word :: Parser Char [Char]
word = some (satisfy isAlpha)

string :: Eq a => [a] -> Parser a [a]
string [] = succeed []
string (x:xs) = (literal x `seq_` string xs) `using` uncurry (:)
