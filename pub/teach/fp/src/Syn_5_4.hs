-- Section 5.4
-- Variable and array declarations (Exercise 5.5)

module Syn_5_4 where
import Scanlib
import Parselib
import Synlib
import Semlib

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
		(number, T_Num),
		(string "begin", T_Sym),
		(string "end", T_Sym),
		(string "var", T_Sym),
		(ident, T_Ide),
		(anyof string ["(",")","+","-","*","/",
				":=",",",";","[","]"],
			T_Sym) ]

-- Syntactic domains
-- Assign Ide Expr is replaced by Assign Expr Expr
data Cmd = Assign Expr Expr | Block [Decl] [Cmd]
data Decl = VarDecl Ide | ArrDecl Ide Numeral
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr
	  | Bexpr BinOpr Expr Expr | Rexpr RelOpr Expr Expr

-- Syntax analysis
decls = many (decl `seq_x` lit ";") `using` concat
decl = lit "var" `x_seq`
	(decl' `seq_` many (lit "," `x_seq` decl') `using` uncurry (:))
decl' = kind T_Ide `seq_` lit "[" `x_seq`
		kind T_Num `seq_x` lit "]"
			`using` uncurry ArrDecl `alt`
	kind T_Ide `using` VarDecl
cmd =	lit "begin" `x_seq` decls `seq_` seqcmd `seq_x` lit "end"
		`using` uncurry Block `alt`
	expr `seq_` lit ":=" `x_seq` expr
		`using` uncurry Assign
seqcmd= cmd `seq_` many (lit ";" `x_seq` cmd) `using` uncurry (:)
expr =	term `seq_` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
		(lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seq_` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
		(lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
	kind T_Ide `seq_` indexed `using` uncurry (flip id) `alt`
	lit "(" `x_seq` expr `seq_x` lit ")"
indexed=lit "[" `x_seq` expr `seq_x` lit "]" `using` flip ArrElem `alt`
	succeed Var

-- Parser
type Prog = Cmd
prog = cmd

lex_ :: [Char] -> [Token Tag]
lex_ = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex_
