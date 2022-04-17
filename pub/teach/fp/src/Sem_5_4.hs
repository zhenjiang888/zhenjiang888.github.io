-- Section 5.4
-- Variable and array declarations (Exercise 5.5)

module Sem_5_4 where
import Syn_5_4
import Semlib
import Domain

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign e e') r s@(l,t) =
		(l, update t k (expval e' r s))
		where
		V_Loc k = explval e r s
cmdexec (Block ds cs) r s@(l,t) = (l, t')
	where
	(r',s') = foldl (flip vardecl) (r,s) ds
	(_,t') = foldl (flip (flip cmdexec r')) s' cs

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
			    where
			    (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
			      where
			      (l',s') = alloc s (numval n)

-- Expression (R-value)
expval :: Expr -> Env -> State -> Val
expval (Num n) r s = V_Int (numval n)
expval (Var x) r (_,t) = lookup_ t k
			 where
			 V_Loc k = lookup_ r x
expval (ArrElem x e) r s@(_,t) = lookup_ t (k + v)
				 where
				 V_Loc k = lookup_ r x
				 V_Int v = expval e r s
expval (Bexpr o e e') r s = V_Int (binopr o v v')
			    where
			    V_Int v = expval e r s
			    V_Int v' = expval e' r s
expval (Rexpr o e e') r s = V_Bool (relopr o v v')
			    where
			    V_Int v = expval e r s
			    V_Int v' = expval e' r s

-- Expression (L-value)
explval :: Expr -> Env -> State -> Val
explval (Num n) r s = undefined
explval (Var x) r s = lookup_ r x
explval (ArrElem x e) r s = V_Loc (k + v)
			    where
			    V_Loc k = lookup_ r x
			    V_Int v = expval e r s
explval (Bexpr o e e') r s = undefined
explval (Rexpr o e e') r s = undefined

-- Storage allocation
alloc :: State -> Int -> (Loc,State)
alloc (l,t) n = (l, (l+n,t'))
	where
	t' = foldl (flip flip undefined . update) t [l,l+n-1]

-- Initial store
t0 :: Store
t0 = none

-- Initial state
s0 :: State
s0 = (1, t0)

-- Initial environment
r0 :: Env
r0 = update none "result" (V_Loc 0)

-- Execution function
exec c = lookup_ (snd s) 0
	 where
	 s = cmdexec (parse c) r0 s0

-- Test 
test = exec "begin var a[10]; a[2]:=3; a[a[2]]:= 2; result:= a[a[2]]+a[a[3]] end"

{- testing result

Sem_5_4> parse "begin var a[10]; a[2]:=3; result:=a[2]+1 end"
Cmd_Block [Decl_ArrDecl "a" "10"] [Cmd_Assign (Expr_ArrElem "a"
(Expr_Num "2")) (Expr_Num "3"),Cmd_Assign (Expr_Var "result")
(Expr_Bexpr BinOpr_Plus (Expr_ArrElem "a" (Expr_Num "2")) (Expr_Num
"1"))]  

Sem_5_4> test
Val_V_Int 5           

-}
