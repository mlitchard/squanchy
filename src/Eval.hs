{-# LANGUAGE FlexibleContexts #-}

module Eval where

import BasicPrelude
import Control.Monad.State
import Control.Monad.Except
import Data.Text
import Data.Typeable
import Debug.Trace
import Types


eval :: Expr a -> EvalMonad 
eval b@(BoolConst _)      = return $ Value b
eval n@(NumberConst _)    = return $ Value n
eval s@(SquanchyString _) = return $ Value s

eval (SquanchyVar v) = extractValue v

eval (Not b)   = do
  b' :: Value <- eval b
  case (castBool b') of
    Just (BoolConst b''') -> return $ Value $ BoolConst $ not b'''
    Just x                -> eval x
    Nothing          -> error ("Not bool error")

eval (And a b) = do
  a' :: Value <- eval a
  b' :: Value <- eval b
  let a'' :: Bool
      (BoolConst a'') = case (castBool a') of
                          Just x  -> x
                          _       -> error ("Expr And - value isn't a bool")
      b'' :: Bool
      (BoolConst b'') = case (castBool b') of
                          Just rb -> rb
                          _       -> error ("Expr And - value isn't a bool")
      res :: Value
      res = Value $ BoolConst $ a'' && b'' 
  return res 

eval (Or a b)  = do
  a' :: Value <- eval a
  b' :: Value <- eval b
  let a'' :: Bool
      (BoolConst a'') = case (castBool a') of
                          Just lb -> lb
                          _       -> error ("Expr Or - value isn't a bool")
      b'' :: Bool
      (BoolConst b'') = case (castBool b') of
                          Just rb -> rb
                          _       -> error ("Expr Or - value isn't a bool")
      res :: Value
      res = Value $  BoolConst $ a'' || b''
  return res  

eval (Xor a b) = do
  a' :: Value <- eval a
  b' :: Value <- eval b
  let a'' :: Bool
      (BoolConst a'') = case (castBool a') of
                          Just lb -> lb
                          Nothing -> error ("Expr Or - l value isn't a bool")
      b'' :: Bool
      (BoolConst b'') = case (castBool b') of
                          Just rb -> rb
                          Nothing -> error ("Expr Or - r value isn't a bool")

      xorRes :: Value
      xorRes = Value $ BoolConst (a'' /= b'')
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
squanchySubtract p q = case (castInt p) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' - q')
    _               -> error $ "can't subtract a float from an int"
  _               -> case (castFloat p) of
    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' - q')
      _               -> error $ "can't subtract an int from a float"
    _                 -> error $ "can't do subtraction"

squanchyAdd :: Value -> Value -> EvalMonad
squanchyAdd p q = case (castInt p) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' + q')
    _               -> error $ "can't add a float and an int"
  _                 -> case (castFloat p) of
    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' + q')
      _                     -> error $ "can't add a float and an int"
    _                     -> error "can't do that"


squanchyDivide :: Value -> Value -> EvalMonad
squanchyDivide p q = case (castInt p) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' `div` q') 
    _                     -> error $ "can't divide a float and an int"
  _                     -> case (castFloat p) of
    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' / q')
      _                     -> error $ "can't divide an int and a float"
    _                     -> error "can't do that"

squanchyMultiply :: Value -> Value -> EvalMonad
squanchyMultiply p q = case (castInt p) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ NumberConst (p' * q')
    _                     -> error
                               $ "can't find equality with float and an int"
  _                     -> case (castFloat p) of
    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ NumberConst ( p' * q')
      _                     -> error
                                 $ "can't find equality with an int and a float"
    _                     -> error "can't do that"

equals :: Value -> Value -> EvalMonad
equals p q = case (castInt p) of
  -- p is an int
  Just (NumberConst p') -> case (castInt q ) of
      -- q is an int
    Just (NumberConst q') -> return $ Value $ BoolConst (p' == q')
    _                     -> error qNotInt

  -- p not an int
  _                     -> case (castFloat p) of
      -- p is a float
    Just (NumberConst p') -> case (castFloat q) of
         -- q is a float
      Just (NumberConst q') -> return $ Value $ BoolConst (p' == q')
      _                      -> error qNotFloat

    _                        -> case (castBool p) of
        -- p is a bool
      Just (BoolConst p') -> case (castBool q) of
        -- q is a bool
        Just (BoolConst q') -> return $ Value $ BoolConst (p' == q')
        _                   -> error qNotBool

      _                   -> case (castText p) of 
        Just (SquanchyString p') -> case (castText q) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' == q')
          _                        -> error qNotString
        _                        -> error "no comparison possible" 

qNotString :: String
qNotString = "nonsensical comparison, p is string q is not"
                                                                         
qNotInt :: String                              
qNotInt = "nonsensical comparison, p is int q is not"

qNotFloat :: String
qNotFloat = "nonsensical comparison, p is float q is not"

qNotBool :: String
qNotBool = "nonsensical comparison, p is bool, q is not"


nonsense :: String
nonsense = "Nonsensical comparison: how did you get this far?"

greaterThan :: Value -> Value -> EvalMonad
greaterThan p q = case (castInt p) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ BoolConst (p' > q')
    _                     -> error $ "can't compare float and an int"

  _                     -> case (castFloat p) of

    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ BoolConst (p' > q')
      _                     -> error $ "can't compare int and a float"

    _                     -> case (castBool p) of
      Just (BoolConst p') -> case (castBool q) of
        Just (BoolConst q') -> return $ Value $ BoolConst (p' > q')
        _                   -> error $ "nonsensical comparison: q not a boolean"

      _                   -> case (castText p) of
        Just (SquanchyString p') -> case (castText q) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' > q')
          _                        -> error
                                        $ "Nonsensical comparison: q not a Text"
        _                        -> error nonsense

lessThan :: Value -> Value -> EvalMonad
lessThan p q = case (castInt p ) of
  Just (NumberConst p') -> case (castInt q) of
    Just (NumberConst q') -> return $ Value $ BoolConst (p' < q')
    _                     -> error $ "can't compare float and an int"

  _                     -> case (castFloat p) of
    Just (NumberConst p') -> case (castFloat q) of
      Just (NumberConst q') -> return $ Value $ BoolConst (p' < q')
      _                     -> error $ "can't compare int and a float"

    _                     -> case (castBool p) of
      Just (BoolConst p') -> case (castBool q) of
        Just (BoolConst q') -> return $ Value $ BoolConst (p' < q')
        _                   -> error $ "nonsensical comparison: q not a boolean"
      _                   -> case (castText p) of
        Just (SquanchyString p') -> case (castText q) of
          Just (SquanchyString q') -> return $ Value $ BoolConst (p' < q')
          _                        -> error
                                        $ "Nonsensical comparison: q not a Text"
        _                          -> error nonsense

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
