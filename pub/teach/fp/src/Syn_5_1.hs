-- Section 5.1
-- Fig.5.2: Assignment and sequencing commands

module Syn_5_1 where
import Scanlib
import Parselib
import Synlib
import Semlib

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
		(number, T_Num),
		(string "begin", T_Sym),
		(string "end", T_Sym),
		(ident, T_Ide),
		(anyof string ["(",")","+","-","*","/",":=",";"], T_Sym) ]

-- Syntactic domains
data Cmd = Assign Ide Expr | Seq [Cmd]
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr

-- Syntax analysis
cmd =	lit "begin" `x_seq` seqcmd `seq_x` lit "end" `using` Seq `alt`
	kind T_Ide `seq_` lit ":=" `x_seq` expr `using` uncurry Assign
seqcmd= cmd `seq_` many (lit ";" `x_seq` cmd) `using` uncurry (:)
expr =	term `seq_` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
		(lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seq_` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
		(lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
	kind T_Ide `using` Var `alt`
	lit "(" `x_seq` expr `seq_x` lit ")"

-- Parser
type Prog = Cmd
prog = cmd

lex_ :: [Char] -> [Token Tag]
lex_ = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex_
