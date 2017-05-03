module Eval where

import BasicPrelude

import Types

eval :: Expr a -> a
eval (BoolConst a)     = a
eval (NumberConst a)   = a

eval (Equals a b)      = (eval a) `equals` (eval b)
eval (GreaterThan a b) = (eval a) `greaterThan` (eval b)
eval (LessThan a b)    = (eval a) `lessThan` (eval b)

eval (Div a b)         = (eval a) `divide` (eval b)
eval (Mul a b)         = (eval a) `mul`    (eval b)
eval (Sub a b)         = (eval a) `sub`    (eval b)
eval (Add a b)         = (eval a) `add`    (eval b)
