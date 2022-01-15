module Statements where

import BasicPrelude

import Types

infix 1 :=

data Stmt a
 = Var := Expr a
 | While (Expr a) (Stmt a)
 | Seq [Stmt a]
