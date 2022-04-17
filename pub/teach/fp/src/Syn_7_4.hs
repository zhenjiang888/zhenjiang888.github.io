-- Section 4.4
-- + Fig.4.3: Expressions with conditionals

module Syn_7_4 where
import Scanlib
import Parselib
import Synlib
import Semlib

-- Note that the order in which strings are enumarated in 'anyof' is
-- significant in case that the same prefices appear in several strings.
lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
		(number, T_Num),
		(string "let", T_Sym),
		(string "in", T_Sym),
		(string "if", T_Sym),
		(string "then", T_Sym),
		(string "else", T_Sym),
		(ident, T_Ide),
		(anyof string ["(",")",
				"==","/=",">=",">","<=","<",
				"+","-","*","/","=",";"
				], T_Sym) ]

-- Syntactic domains
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr | Let [Decl] Expr |
	    Rexpr RelOpr Expr Expr | If Expr (Expr,Expr)
data Decl = Decl Ide Expr

-- Syntax analysis
expr =  lit "if" `x_seq` expr `seq_`
		lit "then" `x_seq` expr `seq_`
		lit "else" `x_seq` expr
			`using` uncurry If `alt`
	lit "let" `x_seq` decls `seq_`
		lit "in" `x_seq` expr
			`using` uncurry Let `alt`
	rexpr
rexpr = expr'' `seq_` rexpr' `using` uncurry (flip id)
rexpr'=	lit "==" `x_seq` expr'' `using` flip (Rexpr Equal) `alt`
	lit "/=" `x_seq` expr'' `using` flip (Rexpr NotEqual) `alt`
	lit ">" `x_seq` expr'' `using` flip (Rexpr Greater) `alt`
	lit ">=" `x_seq` expr'' `using` flip (Rexpr GreaterEq) `alt`
	lit "<" `x_seq` expr'' `using` flip (Rexpr Less) `alt`
	lit "<=" `x_seq` expr'' `using` flip (Rexpr LessEq) `alt`
	succeed id
expr''=	term `seq_` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
		(lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seq_` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
		(lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
	kind T_Ide `using` Var `alt`
	lit "(" `x_seq` expr `seq_x` lit ")"
decls = decl `seq_` many (lit ";" `x_seq` decl) `using` uncurry (:)
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
