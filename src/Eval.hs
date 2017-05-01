module Eval where

import BasicPrelude

import Types

eval :: Expr a -> a
eval (BoolConst a)     = a
eval (NumberConst a)   = a
eval (Equals a b)      = (eval a) `equals` (eval b)
eval (GreaterThan a b) = (eval a) `greaterThan` (eval b)
eval _ = error "unevaluated eval undefined"
