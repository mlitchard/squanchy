{-# LANGUAGE FlexibleContexts #-}

module Eval where

import BasicPrelude
import Control.Monad.State
import Control.Monad.Except
import Data.Text
import Data.Typeable

import Types


eval :: Expr a -> EvalMonad 
eval b@(BoolConst _)      = return $ Value b
eval n@(NumberConst _)    = return $ Value n
eval s@(SquanchyString _) = return $ Value s

eval (SquanchyVar v) = extractValue v
eval (Not b)   = do
  b' :: Value <- eval b
  case (cast b' :: Maybe (Expr Bool)) of
    Just (BoolConst b''') -> return $ Value $ BoolConst $ not b'''
    Just x                -> eval x
    Nothing          -> error ("Not bool error")
--  return $ Value b''
eval (And a b) = do
                    a' :: Value <- eval a
                    b' :: Value <- eval b
                    let a'' :: Bool
                        a'' = case (cast a' :: Maybe (Expr Bool)) of
                                Just (BoolConst lb) -> lb
                                Just x              -> eval x
                                Nothing -> 
                                  error ("Expr And - value isn't a bool")
                        b'' :: Expr Bool
                        b'' = case (cast b' :: Maybe (Expr Bool)) of
                                Just rb -> rb
                                Nothing ->
                                  error ("Expr And - value isn't a bool")
                        res :: Value
                        res = Value $ (&&) <$> a'' <*> b''
                    return res 
eval (Or a b)  = do
                   a' :: Value <- eval a
                   b' :: Value <- eval b
                   let a'' :: Expr Bool
                       a'' = case (cast a' :: Maybe (Expr Bool)) of
                               Just lb -> lb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       b'' :: Expr Bool
                       b'' = case (cast b' :: Maybe (Expr Bool)) of
                               Just rb -> rb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       res :: Value
                       res = Value $ (||) <$> a'' <*> b''
                   return res  
                 -- (||) <$> eval a <*> eval b 
eval (Xor a b) = do
                   a' :: Value <- eval a
                   b' :: Value <- eval b
                   let a'' :: Expr Bool
                       a'' = case (cast a' :: Maybe (Expr Bool)) of
                               Just lb -> lb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       b'' :: Expr Bool
                       b'' = case (cast b' :: Maybe (Expr Bool)) of
                               Just rb -> rb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       orRes :: Expr Bool
                       orRes = (||) <$> a'' <*> b''
                       andRes :: Expr Bool
                       andRes = (&&) <$> a'' <*> b''
                       notRes :: Expr Bool
                       notRes = not <$> andRes
                       xorRes :: Value
                       xorRes = Value $ (&&) <$> orRes <*> orRes
                   return xorRes 
--                  orRes :: Bool <- (||) <$> eval a <*> eval b
--                  andRes :: Bool <- (&&) <$> eval a <*> eval b
--                  let notRes = not andRes
--                  return $ orRes && notRes
{-
eval (Equals a b) = equals a b
eval (GreaterThan a b) = (>) <$> eval a <*> eval b
eval (LessThan a b)    = (<) <$> eval a <*> eval b

eval (Div a b) = divide <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b 
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
-}
eval _             = undefined

equals :: (Eq a) => Expr a -> Expr a -> EvalMonad
equals a b = do
  a' <- eval a
  b' <- eval b
  let a'' :: Expr Bool
      a'' = case (cast a' :: Maybe (Expr Bool)) of
              Just lb -> lb
              Nothing -> error ("Expr Or - value isn't a bool")
      b'' :: Expr Bool
      b'' = case (cast b' :: Maybe (Expr Bool)) of
              Just rb -> rb
              Nothing -> error ("Expr Or - value isn't a bool")

  return $ Value $ (==) <$> a'' <*> b''

extractValue :: Text -> EvalMonad
extractValue v = do
    store :: Store <- lift get
    case (lookup v store) of
      Just i -> do
                  let mEval = toExpr i
                  case mEval of
                    (Just e) -> eval e
                    Nothing  -> throwError ("this should never happen")
      Nothing -> throwError "doh"
