-- Section 4.2
-- Expressions with Declarations

module Sem_4_2 where
import Syn_4_2
import Semlib

type Val = Int
type Env = Assoc Ide Val

expval :: Expr -> Env -> Val
expval (Num n) r = numval n
expval (Var x) r = lookup_ r x
expval (Bexpr o e e') r = binopr o (expval e r) (expval e' r)
expval (Let d e) r = expval e (declenv d r r)

declenv :: Decl -> Env -> Env -> Env
declenv (Decl x e) r' r = update r x (expval e r')

-- Initial environment
r0 :: Env
r0 = none

-- Evaluation function
eval e = expval (parse e) r0

{- test

Sem_4_2> parse "let x=29+31 in x*3-x*x"
Expr_Let (Decl_Decl "x" (Expr_Bexpr BinOpr_Plus (Expr_Num "29") (Expr_Num "31"))) (Expr_Bexpr BinOpr_Minus (Expr_Bexpr BinOpr_Times (Expr_Var "x") (Expr_Num "3" )) (Expr_Bexpr BinOpr_Times (Expr_Var "x") (Expr_Var "x")))

Sem_4_2> eval "let x=29+31 in x*3-x*x"
-3420               

-}
