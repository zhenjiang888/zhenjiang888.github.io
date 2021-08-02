-- Semantic domains for imperative languages

module Domain where
import Semlib

data Val = V_Int Int | V_Bool Bool | V_Loc Loc
	 | V_Cont Cont | V_Proc Proc | V_Func Func
	 | V_Thunk (E_Cont -> Cont)
type Loc = Int
type Store = Assoc Loc Val
type State = (Loc, Store)
type Env = Assoc Ide Val
type Ans = [Val]
type Cont = State -> Ans
type E_Cont = Val -> Cont
type Proc = [Val] -> Cont -> Cont
type Func = [Val] -> E_Cont -> Cont
type Thunk = E_Cont -> Cont
