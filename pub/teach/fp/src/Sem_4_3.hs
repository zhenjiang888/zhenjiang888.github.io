-- Section 4.3
-- Expressions with Declaration lists

module Sem_4_3 where
import Syn_4_3
import Semlib

type Env = Assoc Ide Int

expval :: Expr -> Env -> Int
expval (Num n) r = numval n
expval (Var x) r = lookup_ r x
expval (Bexpr o e e') r = binopr o (expval e r) (expval e' r)
--expval (Let ds e) r = expval e r''
--		      where
--		      r'' = foldl declenv' r ds
--		      declenv' r' d = declenv d r r'
expval (Let ds e) r = expval e (recdecl ds r)

declenv :: Decl -> Env -> Env -> Env
declenv (Decl x e) r' r = update r x (expval e r')

recdecl :: [Decl] -> Env -> Env
recdecl ds r = r''
	       where
	       r'' = foldl declenv' r ds
	       declenv' r' d = declenv d r'' r'

-- Initial environment
r0 :: Env
r0 = none

-- Evaluation function
eval e = expval (parse e) r0

{- test

Sem_4_3> parse "let x=2 in (let x=3 in x)"
Expr_Let [Decl_Decl "x" (Expr_Num "2")] (Expr_Let [Decl_Decl "x" (Expr_Num "3")] (Expr_Var "x"))

Sem_4_3> parse "let x=2;y=3 in x+y"
Expr_Let [Decl_Decl "x" (Expr_Num "2"),Decl_Decl "y" (Expr_Num "3")] (Expr_Bexpr BinOpr_Plus (Expr_Var "x") (Expr_Var "y"))  

Sem_4_3> eval "let x=1 in (let y=x;x=2 in x+y)"
4

-}
