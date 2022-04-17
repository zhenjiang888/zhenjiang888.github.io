-- Section 7.2 Type checking

module Sem_7_4 where
import Syn_7_4
import Semlib
import Domain

data Typ = Typ_Int | Typ_Bool | Typ_Wrong
type Typ_Env = Assoc Ide Typ

exptyp :: Expr -> Typ_Env -> Typ
exptyp (Num n) r = Typ_Int
exptyp (Var x) r = lookup_ r x
exptyp (Let ds e) r = exptyp e (foldl declenv' r ds)
	where
	declenv' r' (Decl x e) = update r' x (exptyp e r)
exptyp (Bexpr o e e') r
	= bintyp (exptyp e r) (exptyp e' r)
	where
	bintyp Typ_Int Typ_Int = Typ_Int
	bintyp _ _ = Typ_Wrong
exptyp (Rexpr o e e') r
	= reltyp (exptyp e r) (exptyp e' r)
	where
	reltyp Typ_Int Typ_Int = Typ_Bool
	reltyp _ _ = Typ_Wrong
exptyp (If e (e',e'')) r
	= iftyp (exptyp e r) (exptyp e' r) (exptyp e'' r)
	where
	iftyp Typ_Bool Typ_Int Typ_Int = Typ_Int
	iftyp Typ_Bool Typ_Bool Typ_Bool = Typ_Bool
	iftyp _ _ _ = Typ_Wrong

-- Initial environment
r0 :: Typ_Env
r0 = assoc Typ_Wrong

-- Typing function
typ :: [Char] -> Typ
typ e = exptyp (parse e) r0

{- test

Sem_7_4> typ "291+31"
Typ_Typ_Int
Sem_7_4> typ "291<31"
Typ_Typ_Bool
Sem_7_4> typ "let x=2 in let y=x+1; z=x in x+y+z"
Typ_Typ_Int
Sem_7_4> typ "let x=0 in if x==0 then x==0 else x-1"
Typ_Typ_Wrong          

-}