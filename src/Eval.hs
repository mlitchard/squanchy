{-# LANGUAGE FlexibleContexts #-}

module Eval where

import BasicPrelude
import Control.Monad.State
import Control.Monad.Except
import Data.Text
import Types


eval :: Expr a -> EvalMonad a a
eval (BoolConst a)      = return a
eval (NumberConst a)    = return a
eval (SquanchyString s) = return s

eval (SquanchyVar v) = extractValue v
eval (Not b)   = not <$> eval b 
eval (And a b) = (&&) <$> eval a <*> eval b 
eval (Or a b)  = (||) <$> eval a <*> eval b 
eval (Xor a b) = do
                  orRes :: Bool <- (||) <$> eval a <*> eval b
                  andRes :: Bool <- (&&) <$> eval a <*> eval b
                  let notRes = not andRes
                  return $ orRes && notRes
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

equals :: (Eq a) => Expr a -> Expr a -> EvalMonad a Bool
equals a b = do
  eOne <- eval a
  eTwo <- eval b
  return $ eOne == eTwo

extractValue :: Text -> EvalMonad a a
extractValue v = do
    store :: Store a <- lift get
    case (lookup v store) of
      Just i -> eval i
      Nothing -> throwError "doh"
