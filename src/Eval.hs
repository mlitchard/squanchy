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

eval (SquanchyVar v) = undefined
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
                        (BoolConst a'') = case (cast a' :: Maybe (Expr Bool)) of
                                Just x  -> x
                                Nothing -> 
                                  error ("Expr And - value isn't a bool")
                        b'' :: Bool
                        (BoolConst b'') = case (cast b' :: Maybe (Expr Bool)) of
                                Just rb -> rb
                                Nothing ->
                                  error ("Expr And - value isn't a bool")
                        res :: Value
                        res = Value $ BoolConst $ a'' && b'' 
                    return res 
eval (Or a b)  = do
                   a' :: Value <- eval a
                   b' :: Value <- eval b
                   let a'' :: Bool
                       (BoolConst a'') = case (cast a' :: Maybe (Expr Bool)) of
                               Just lb -> lb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       b'' :: Bool
                       (BoolConst b'') = case (cast b' :: Maybe (Expr Bool)) of
                               Just rb -> rb
                               Nothing ->
                                 error ("Expr Or - value isn't a bool")
                       res :: Value
                       res = Value $  BoolConst $ a'' || b''
                   return res  
                 -- (||) <$> eval a <*> eval b 
eval (Xor a b) = do
                   a' :: Value <- eval a
                   b' :: Value <- eval b
                   let a'' :: Bool
                       (BoolConst a'') = case (cast a' :: Maybe (Expr Bool)) of
                          Just lb -> lb
                          Nothing -> error ("Expr Or - value isn't a bool")
                       b'' :: Bool
                       (BoolConst b'') = case (cast b' :: Maybe (Expr Bool)) of
                          Just rb -> rb
                          Nothing -> error ("Expr Or - value isn't a bool")
                       orRes :: Bool
                       orRes = a'' || b'' 
                       andRes :: Bool
                       andRes = a'' || b'' 
                       notRes :: Bool
                       notRes = not andRes
                       xorRes :: Value
                       xorRes = Value $ BoolConst $ orRes && notRes
                   return xorRes 

eval (Equals p q)      = do
                           p' :: Value <- eval p
                           q' :: Value <- eval q
                           equals p' q'

eval (GreaterThan p q) = do
                           p' :: Value <- eval p
                           q' :: Value <- eval q
                           greaterThan p' q'

eval (LessThan p q)    = do
                           p' :: Value <- eval p
                           q' :: Value <- eval q
                           lessThan p' q'

eval (Div p q) = do
                   p' :: Value <- eval p 
                   q' :: Value <- eval q 
                   squanchyDivide p' q' 
eval (Mul p q) = do
                   p' :: Value <- eval p
                   q' :: Value <- eval q
                   squanchyMultiply p' q'

eval (Add p q) = do
                   p' :: Value <- eval p
                   q' :: Value <- eval q
                   squanchyAdd p' q'

eval (Sub p q) = do
                   p' :: Value <- eval p
                   q' :: Value <- eval q
                   squanchySubtract p' q'


-- BUGFIX Dividable TypeClass rendered useless
squanchySubtract :: Value -> Value -> EvalMonad
squanchySubtract p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' - q')
    Nothing               -> error
                               $ "can't find equality with float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' - q')
      Nothing               -> error
                                 $ "can't find equality with an int and a float"
    Nothing -> error "can't do that"

squanchyAdd :: Value -> Value -> EvalMonad
squanchyAdd p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' + q')
    Nothing               -> error
                               $ "can't find equality with float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' + q')
      Nothing               -> error
                                 $ "can't find equality with an int and a float"
    Nothing -> error "can't do that"


squanchyDivide :: Value -> Value -> EvalMonad
squanchyDivide p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' `div` q') 
    Nothing               -> error
                               $ "can't find equality with float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' / q')
      Nothing               -> error
                                 $ "can't find equality with an int and a float"
    Nothing -> error "can't do that"

squanchyMultiply :: Value -> Value -> EvalMonad
squanchyMultiply p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' * q')
    Nothing               -> error
                               $ "can't find equality with float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' * q')
      Nothing               -> error
                                 $ "can't find equality with an int and a float"
    Nothing -> error "can't do that"

equals :: Value -> Value -> EvalMonad
equals p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ BoolConst (p' == q')
    Nothing               -> error
                               $ "can't find equality with float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ BoolConst (p' == q')
      Nothing               -> error
                                 $ "can't find equality with an int and a float"
    Nothing -> case (cast p :: Maybe (Expr Bool)) of
      Just (BoolConst p') -> case (cast q :: Maybe (Expr Bool)) of
        Just (BoolConst q') -> return $ Value $ BoolConst (p' == q')
        Nothing -> error
                     $ "nonsensical equality: q not a boolean"
      Nothing -> case (cast p :: Maybe (Expr Text)) of
        Just (SquanchyString p') -> case (cast q :: Maybe (Expr Text)) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' == q')
          Nothing -> error
                       $ "Nonsensical equality: q not a Text"
        Nothing -> error "Nonsensical equality: how did you get this far?"

greaterThan :: Value -> Value -> EvalMonad
greaterThan p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ BoolConst (p' > q')
    Nothing               -> error
                               $ "can't compare float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ BoolConst (p' > q')
      Nothing               -> error
                                 $ "can't compare int and a float"
    Nothing -> case (cast p :: Maybe (Expr Bool)) of
      Just (BoolConst p') -> case (cast q :: Maybe (Expr Bool)) of
        Just (BoolConst q') -> return $ Value $ BoolConst (p' > q')
        Nothing -> error
                     $ "nonsensical comparison: q not a boolean"
      Nothing -> case (cast p :: Maybe (Expr Text)) of
        Just (SquanchyString p') -> case (cast q :: Maybe (Expr Text)) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' > q')
          Nothing -> error
                       $ "Nonsensical comparison: q not a Text"
        Nothing -> error "Nonsensical comparison: how did you get this far?"

lessThan :: Value -> Value -> EvalMonad
lessThan p q = case (cast p :: Maybe (Expr Int)) of
  Just (NumberConst p') -> case (cast q :: Maybe (Expr Int)) of
    Just (NumberConst q') -> return $ Value $ BoolConst (p' < q')
    Nothing               -> error
                               $ "can't compare float and an int"
  Nothing               -> case (cast p :: Maybe (Expr Float)) of
    Just (NumberConst p') -> case (cast q :: Maybe (Expr Float)) of
      Just (NumberConst q') -> return $ Value $ BoolConst (p' < q')
      Nothing               -> error
                                 $ "can't compare int and a float"
    Nothing -> case (cast p :: Maybe (Expr Bool)) of
      Just (BoolConst p') -> case (cast q :: Maybe (Expr Bool)) of
        Just (BoolConst q') -> return $ Value $ BoolConst (p' < q')
        Nothing -> error
                     $ "nonsensical comparison: q not a boolean"
      Nothing -> case (cast p :: Maybe (Expr Text)) of
        Just (SquanchyString p') -> case (cast q :: Maybe (Expr Text)) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' < q')
          Nothing -> error
                       $ "Nonsensical comparison: q not a Text"
        Nothing -> error "Nonsensical comparison: how did you get this far?"

extractValue :: Text -> EvalMonad
extractValue v = do
  store :: Store <- lift get
  case (lookup v store :: Maybe Value) of
    Just i -> do
      case (toExpr i :: Maybe (Expr Int)) of
        Just i' -> return $ Value i'
        Nothing -> case (toExpr i :: Maybe (Expr Float)) of
          Just i' -> return $ Value i'
          Nothing -> case (toExpr i :: Maybe (Expr Bool)) of
            Just i' -> return $ Value i'
            Nothing -> case (toExpr i :: Maybe (Expr Text)) of
              Just i' -> return $ Value i'
              Nothing -> error "should never have gotten here"
    Nothing -> error "should never have gotten here"
