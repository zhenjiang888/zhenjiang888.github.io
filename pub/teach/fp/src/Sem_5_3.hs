-- Section 5.3
-- Fig.5.7: Variable declarations and storage allocation

module Sem_5_3 where
import Syn_5_3
import Semlib
import Domain

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign x e) r s@(l,t) = (l, update t k (expval e r s))
		where
		V_Loc k = lookup_ r x
cmdexec (Block ds cs) r s@(l,t) = (l, t')
	where
	(r',s') = foldl (flip vardecl) (r,s) ds
	(_,t') = foldl (flip (flip cmdexec r')) s' cs

vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
			    where
			    (l',s') = alloc s 1
	

expval :: Expr -> Env -> State -> Val
expval (Num n) r s = V_Int (numval n)
expval (Var x) r (_,t) = lookup_ t k
			 where
			 V_Loc k = lookup_ r x
expval (Bexpr o e e') r s = V_Int (binopr o v v')
			    where
			    V_Int v = expval e r s
			    V_Int v' = expval e' r s

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

{- test

Sem_5_3> exec "begin var y; y:=1; result:=y+2 end"
Val_V_Int 3

Sem_5_3> parse "begin var y; y:=1; begin var y; y:=2; result:=y+2 end end"
Cmd_Block [Decl_VarDecl "y"] [Cmd_Assign "y" (Expr_Num "1"),Cmd_Block
[Decl_VarDecl "y"] [Cmd_Assign "y" (Expr_Num "2"),Cmd_Assign "result"
(Expr_Bexpr BinOpr_Plus (Expr_Var "y") (Expr_Num "2"))]] 

Sem_5_3> exec "begin var y; y:=1; begin var y; y:=2; result:=y+2 end end"
Val_V_Int 4

-}
