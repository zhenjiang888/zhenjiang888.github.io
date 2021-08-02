-- Section 4.4
-- Fig.4.3: Expressions with conditionals

module Sem_4_4 where
import Syn_4_4
import Semlib

data Val = V_Int Int | V_Bool Bool

type Env = Assoc Ide Val

expval :: Expr -> Env -> Val
expval (Num n) r = V_Int (numval n)
expval (Var x) r = lookup_ r x
expval (Let ds e) r = expval e (recdecl ds r)
expval (Bexpr o e e') r = V_Int(binopr o v v')
			  where
			  V_Int v = expval e r
			  V_Int v' = expval e' r
expval (Rexpr o e e') r = V_Bool (relopr o v v')
			  where
			  V_Int v = expval e r
			  V_Int v' = expval e' r
expval (If e (e',e'')) r | v = expval e' r
			 | otherwise = expval e'' r
			 where V_Bool v = expval e r

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

Sem_4_4> parse "if n==0 then 1 else n*2"
Expr_If (Expr_Rexpr RelOpr_Equal (Expr_Var "n") (Expr_Num "0")) (Expr_Num "1",Expr_Bexpr BinOpr_Times (Expr_Var "n") (Expr_Num "2"))

Sem_4_4> eval "let n=3 in if n==0 then 1 else n*2"
Val_V_Int 6          

-}