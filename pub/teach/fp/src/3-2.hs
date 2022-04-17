-- Alphabet
data Digit = Digit_0 | Digit_1 | Digit_2 | Digit_3 | Digit_4
	   | Digit_5 | Digit_6 | Digit_7 | Digit_8 | Digit_9

-- Language
data Numeral = Single Digit | Composite Numeral Digit

-- Valuation function
numval :: Numeral -> Int
numval (Single d) = digval d
numval (Composite n d) = numval n * 10 + digval d

-- Auxiliary function
digval :: Digit -> Int
digval Digit_0 = 0
digval Digit_1 = 1
digval Digit_2 = 2
digval Digit_3 = 3
digval Digit_4 = 4
digval Digit_5 = 5
digval Digit_6 = 6
digval Digit_7 = 7
digval Digit_8 = 8
digval Digit_9 = 9

-- Abstract syntax of expressions
data Expr = Num Numeral | Pexpr Expr Expr | Mexpr Expr Expr

-- Valuation function
expval :: Expr -> Int
expval (Num n) = numval n
expval (Pexpr e1 e2) = expval e1 + expval e2
expval (Mexpr e1 e2) = expval e1 - expval e2

-- Test
exp1 = Num (Composite (Single Digit_3) Digit_1)
exp2 = Pexpr (Num (Composite (Single Digit_3)
                             Digit_1))
             (Num(Single Digit_5))
