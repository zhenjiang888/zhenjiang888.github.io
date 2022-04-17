-- Section 6.1

module Syn_6_1 where
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
		(string "if", T_Sym),
		(string "then", T_Sym),
		(string "else", T_Sym),
		(string "while", T_Sym),
		(string "do", T_Sym),
		(string "skip", T_Sym),
		(ident, T_Ide),
		(anyof string ["(",")",
				"==","/=",">=",">","<=","<",
				"+","-","*","/",
				":=",",",";","[","]"
				], T_Sym)]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] | If Expr (Cmd,Cmd)
	 | While Expr Cmd | Skip
data Decl = VarDecl Ide | ArrDecl Ide Numeral
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr | Bexpr BinOpr Expr Expr
	  | Rexpr RelOpr Expr Expr

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
	lit "if" `x_seq` expr `seq_`
		(lit "then" `x_seq` cmd `seq_` (lit "else" `x_seq` cmd ))
			`using` uncurry If `alt`
	lit "while" `x_seq` expr `seq_` (lit "do" `x_seq` cmd)
			`using` uncurry While `alt`
	lit "skip" `using` const Skip `alt`
	expr `seq_` lit ":=" `x_seq` expr `using` uncurry Assign
seqcmd= cmd `seq_` many (lit ";" `x_seq` cmd) `using` uncurry (:)

expr = expr'' `seq_` rexpr' `using` uncurry (flip id)
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
