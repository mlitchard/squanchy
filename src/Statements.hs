module Statements where

import qualified Data.Map.Lazy as M
import BasicPrelude
import Control.Monad.State
import Data.Maybe
import Data.Typeable

import Eval
import Types

infix 1 :=

type Prog = Stmt

data Stmt 
 = Var := Value
 | While Value Stmt
 | Seq [Stmt]

exec :: Stmt -> EvalMonad ()
exec (x := e)      = modify $ M.insert x e
exec (While e s)   = do
                        let expr :: (Maybe (Expr Bool))
                            expr = toExpr e 
                        case expr of
                          Just (BoolConst res) -> do
                                                   when (not res)    
                                                     $ exec (Seq [s, While e s])
                          _      -> error whileError 
exec (Seq [])      = return ()
exec (Seq (s:ss))  = do
                       exec s
                       exec (Seq ss)

whileError :: String
whileError = "While needs a boolean to evaluate"
{-
fib :: Prog Value
fib = Seq
  [ "x" := NumberConst (0 :: Int)]
-}
