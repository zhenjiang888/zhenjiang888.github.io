-- Lexical elements and lexical analysis

module Synlib where
import Parselib
import Scanlib

-- Lexical elements
data Tag = T_Num | T_Ide | T_Sym | T_Junk

instance Eq Tag where
	T_Num==T_Num = True
	T_Ide==T_Ide = True
	T_Sym==T_Sym = True
	T_Junk==T_Junk = True
	_ == _ = False

-- Lexical analysis
lit :: [Char] -> Parser (Token Tag) [Char]
lit xs = literal (T_Sym,xs) `using` snd

ident :: Parser Char [Char]
ident = (satisfy isAlpha `seq_` many (satisfy isAlphaNum)) `using` uncurry (:)





