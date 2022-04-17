-- Section 4.2
-- Expressions with Declarations

module Syn_4_2 where
import Scanlib
import Parselib
import Synlib
import Semlib

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
		(number, T_Num),
		(string "let", T_Sym),
		(string "in", T_Sym),
		(ident, T_Ide),
		(anyof string ["(",")","+","-","*","/","="], T_Sym) ]

-- Syntactic domains
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr | Let Decl Expr
data Decl = Decl Ide Expr

-- Syntax analysis
expr =	lit "let" `x_seq` decl `seq_` lit "in" `x_seq` expr
		`using` uncurry Let `alt`
	expr''
expr''=	term `seq_` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
		(lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seq_` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
		(lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
	kind T_Ide `using` Var `alt`
	lit "(" `x_seq` expr `seq_x` lit ")"
decl =	kind T_Ide `seq_` lit "=" `x_seq` expr `using` uncurry Decl

-- Parser
type Prog = Expr
prog = expr

lex_ :: [Char] -> [Token Tag]
lex_ = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex_
