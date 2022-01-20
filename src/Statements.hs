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

exec :: Prog -> EvalMonad ()
exec (var := val)  = modify $ M.insert var val
exec (While e s)   = do
  let expr :: (Maybe (Expr Bool))
      expr = toExpr e 
  case expr of
    Just res -> do
                  res' :: Value <- eval res
                  let expr' :: (Maybe (Expr Bool))
                      expr' = castBool res'
                     
                  case expr' of
                    Just (BoolConst b) -> when (not b) 
                                            $ exec (Seq [s, While e s])
                    p                  -> error $ reductionError p
    _      -> error whileError 
exec (Seq [])      = return ()
exec (Seq (s:ss))  = do
                       exec s
                       exec (Seq ss)

whileError :: String
whileError = "While needs a boolean to evaluate"

reductionError :: Maybe (Expr Bool) -> String
reductionError Nothing = "Not a bool"
reductionError (Just (Sub _ _)) = "it never reduced the sub"
reductionError x'' = 
  "While couldn't reduce the boolean value for some reason " 
    ++ (show $ typeOf x'')

zero :: Value
zero = Value $ NumberConst (0 :: Int)

one :: Value
one = Value $ NumberConst (1 :: Int)

booleanTest :: Value
booleanTest = Value $ (Not (Equals (SquanchyVar "n") (NumberConst (1 :: Int))))

x :: Expr Int
x = SquanchyVar "x"

y :: Expr Int
y = SquanchyVar "y"

z :: Expr Int
z = SquanchyVar "z"

n :: Expr Int
n = SquanchyVar "n"

fib :: Prog
fib = Seq
  [ "x" := zero
  , "y" := one
  , While booleanTest $ Seq 
      [ "z" := (Value (Add x y)) 
      , "x" := (Value y)
      , "y" := (Value z)
      , "n" := (Value ((Sub n (NumberConst 1 :: Expr Int))))    
      ]
       
 ]

