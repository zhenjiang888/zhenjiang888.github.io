-- Section 5.1
-- Fig.5.2: Assignment and sequencing commands

module Sem_5_1 where
import Syn_5_1
import Semlib

data Val = V_Int Int
type Store = Assoc Ide Val

cmdexec :: Cmd -> Store -> Store
cmdexec (Assign x e) s = update s x (expval e s)
cmdexec (Seq cs) s = foldl (flip cmdexec) s cs

expval :: Expr -> Store -> Val
expval (Num n) s = V_Int (numval n)
expval (Var x) s = lookup_ s x
expval (Bexpr o e e') s = V_Int (binopr o v v')
			  where
			  V_Int v = expval e s
			  V_Int v' = expval e' s


-- Initial store
t0 :: Store
t0 = none

-- Execution function
exec c = lookup_ (cmdexec (parse c) t0) "result"

{- test 

Sem_5_1>  parse "begin y:=1; result:=y+2 end"
Cmd_Seq [Cmd_Assign "y" (Expr_Num "1"),Cmd_Assign "result" (Expr_Bexpr
BinOpr_Pl us (Expr_Var "y") (Expr_Num "2"))]   

Sem_5_1> exec "begin y:=1; result:=y+2 end"
Val_V_Int 3               

-}