{-# LANGUAGE FlexibleContexts #-}
module Eval where

import BasicPrelude

import Types

eval :: Expr a -> a
eval (BoolConst a)     = a
eval (NumberConst a)   = a
eval (SquanchyString a) = a
eval (SquanchyVar name val) = (,) name (eval val)
eval (Not a)           = not (eval a)
eval (And a b)         = (eval a) && (eval b)
eval (Or a b)          = (eval a) || (eval b)
eval (Xor a b)         = (eval a) || (eval b) && (not (eval a && eval b))

eval (Equals a b)      = (eval a) == (eval b)
eval (GreaterThan a b) = (eval a) > (eval b)
eval (LessThan a b)    = (eval a) < (eval b)

eval (Div a b)         = (eval a) `divide` (eval b)
eval (Mul a b)         = (eval a) `mul`    (eval b)
eval (Sub a b)         = (eval a) `sub`    (eval b)
eval (Add a b)         = (eval a) `add`    (eval b)
