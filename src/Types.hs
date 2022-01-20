{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Text
import Data.Typeable

import BasicPrelude


type Var = Text
type Interp = Store -> Either String (Value, Store) 
type Store = Map Var Value
type EvalMonad a = ExceptT Text (State Store) a

data Value where
  Value :: Typeable a => Expr a -> Value

toExpr :: Typeable a => Value -> Maybe (Expr a)
toExpr (Value e) = cast e

showValue :: Value -> Maybe String
showValue (Value v) = case (cast v :: Maybe (Expr Bool)) of
  Just (BoolConst b) -> Just $ show b
  _                  -> case (cast v :: Maybe (Expr Text)) of
    Just (SquanchyString t) -> Just $ unpack t
    _                       -> case (cast v :: Maybe (Expr Int)) of
      Just (NumberConst i) -> Just $ show i
      _                    -> case (cast v :: Maybe (Expr Float)) of
        Just (NumberConst fl) -> Just $ show fl
        _                     -> Nothing 

castBool :: Value -> Maybe (Expr Bool)
castBool (Value a) = cast a
  
evalBool :: (Typeable a) => Expr a -> Maybe Bool
evalBool (BoolConst a) = cast a
evalBool (Not a)       = evalBool a 
evalBool (And a b)     = (&&) <$> evalBool a <*> evalBool b
evalBool (Or  a b)     = (||) <$> evalBool a <*> evalBool b
evalBool (Xor a b)     = (/=) <$> evalBool a <*> evalBool b
evalBool (Equals a b)  = (==) <$> evalBool a <*> evalBool b
evalBool _             = Nothing

castInt :: Value -> Maybe (Expr Int)
castInt (Value a) = cast a 

castFloat :: Value -> Maybe (Expr Float)
castFloat (Value a) = cast a

castText :: Value -> Maybe (Expr Text)
castText (Value a) = cast a

-- ExceptT Text (State (Store a)) (Expr a)
class (Num a, Eq a, Ord a, Typeable a) => Divisible a where
  divide :: a -> a -> a

instance Divisible Int where divide = div
instance Divisible Float where divide = (/)

data Expr a where
  BoolConst      ::  Bool -> Expr Bool
  NumberConst    :: (Divisible n) => n -> Expr n
  SquanchyString :: Text -> Expr Text
  SquanchyVar    :: Text -> Expr a

  Not         :: Expr Bool -> Expr Bool
  And         :: Expr Bool -> Expr Bool -> Expr Bool
  Or          :: Expr Bool -> Expr Bool -> Expr Bool
  Xor         :: Expr Bool -> Expr Bool -> Expr Bool
  
  Equals      :: (Eq a, Typeable a)  => Expr a -> Expr a -> Expr Bool
  GreaterThan :: (Ord a, Typeable a) => Expr a -> Expr a -> Expr Bool
  LessThan    :: (Ord a, Typeable a) => Expr a -> Expr a -> Expr Bool

  Div         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Mul         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Sub         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Add         :: (Divisible n) => Expr n -> Expr n -> Expr n
