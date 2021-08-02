-- Section 6.1
-- Fig.6.2: Control structures

module Sem_6_1 where
import Syn_6_1
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
	(_, t') = foldl (flip (flip cmdexec r')) s' cs
cmdexec (If e (c,c')) r s
	| v = cmdexec c r s
	| otherwise = cmdexec c' r s
	where
	V_Bool v = expval e r s
cmdexec c@(While e c') r s
	| v = cmdexec c r (cmdexec c' r s)
	| otherwise = s
	where
	V_Bool v = expval e r s
cmdexec Skip r s = s

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

-- Testing

t = exec testprog
testprog ="begin var n,f; " ++
          " n:=5; f:=1; " ++
          " while n>0 do " ++
          "  begin f:=f*n; n:=n-1 end; " ++
          " result:=f " ++
          "end"

{- test

Sem_6_1> parse "if x<y then begin z:=x; x:=y; y:=z end else skip"
Cmd_If (Expr_Rexpr RelOpr_Less (Expr_Var "x") (Expr_Var "y"))
(Cmd_Block [] [Cmd Assign (Expr_Var "z") (Expr_Var "x"),Cmd_Assign
(Expr_Var "x") (Expr_Var "y"),Cmd_Assign (Expr_Var "y") (Expr_Var
"z")],Cmd_Skip) 

Sem_6_1> parse "while n>0 do begin f:=f*n; n:=n-1 end"
Cmd_While (Expr_Rexpr RelOpr_Greater (Expr_Var "n") (Expr_Num "0"))
(Cmd_Block [] [Cmd_Assign (Expr_Var "f") (Expr_Bexpr BinOpr_Times
(Expr_Var "f") (Expr_Var "n")),Cmd_Assign (Expr_Var "n") (Expr_Bexpr
BinOpr_Minus (Expr_Var "n") (Expr_Num "1"))])

Sem_6_1> t
Val_V_Int 120  

-}