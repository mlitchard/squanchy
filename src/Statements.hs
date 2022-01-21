module Statements where

import qualified Data.Map.Lazy as M
import BasicPrelude
import Control.Monad.State
import Data.Typeable

import Eval
import Types

infix 1 :=

type Prog = Stmt

data Stmt 
 = Var := Value
 | While Value Stmt
 | Seq [Stmt]


doStore :: Text -> Value -> EvalMonad ()
doStore var val = do
  case (castInt val) of
    Just intExpr -> do
                      res <- eval intExpr
                      case (castInt res) of
                        Just (NumberConst _) -> modify $ M.insert var res
                        _ -> error "illegal value"
    Nothing      -> do
      case (castFloat val) of
        Just floatExpr -> do
                            res <- eval floatExpr
                            case (castFloat res) of     
                              Just (NumberConst _) -> modify $ M.insert var res
                              _ -> error "illegal value"
        Nothing -> case (castBool val) of
          (Just boolExpr) -> do
                               res <- eval boolExpr
                               case (castBool res) of
                                 Just (BoolConst _) -> modify $ M.insert var res
                                 _ -> error "illegal value"
          Nothing         -> do
            case (castText val) of
              (Just textExpr) -> do
                                   res <- eval textExpr
                                   case (castText res) of
                                     Just (SquanchyString _) -> 
                                       modify $ M.insert var res 

                                     _  -> error "illegal value"
              Nothing -> error "illegal value"                        
                                   
          
 
exec :: Prog -> EvalMonad ()
exec (var := val)  = doStore var val
exec (While e s)   = do
  let expr :: (Maybe (Expr Bool))
      expr = toExpr e 
  case expr of
    Just res -> do
                  res' :: Value <- eval res
                  let expr' :: (Maybe (Expr Bool))
                      expr' = castBool res'
                     
                  case expr' of
                    Just (BoolConst b) -> when b
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

