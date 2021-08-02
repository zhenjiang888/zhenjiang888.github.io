-- Section 4.5
-- Expressions with Functions

module Sem_4_5 where
import Syn_4_5
import Semlib

data Val = V_Int Int | V_Bool Bool | V_Fun (Val -> Val)

type Env = Assoc Ide Val

expval :: Expr -> Env -> Val
expval (Num n) r = V_Int (numval n)
expval (Var x) r = lookup_ r x
expval (Let ds e) r = expval e (recdecl ds r)
expval (Bexpr o e e') r = V_Int (binopr o v v')
			  where
			  V_Int v = expval e r
			  V_Int v' = expval e' r
expval (Rexpr o e e') r = V_Bool (relopr o v v')
			  where
			  V_Int v = expval e r
			  V_Int v' = expval e' r
expval (If e (e',e'')) r | v = expval e' r
			 | otherwise = expval e'' r
			 where
			 V_Bool v = expval e r
expval (Fun x e) r = V_Fun f
		     where
		     f v = expval e (update r x v)
expval (Apply e e') r = f (expval e' r)
			where
			V_Fun f = expval e r

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

Sem_4_5>  parse "let square x=x*x in square 5"
Expr_Let [Decl_Decl "square" (Expr_Fun "x" (Expr_Bexpr BinOpr_Times (Expr_Var "x") (Expr_Var "x")))] (Expr_Apply (Expr_Var "square") (Expr_Num "5"))

Sem_4_5> eval "let f n=if n==0 then 1 else n*f(n-1) in f 5"
Val_V_Int 120                  

-}