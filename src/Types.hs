{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Text

import BasicPrelude

type Var = Text
type Interp a = Store a -> Either String (a, Store a) 
type Store a = [(Var, Expr a)]
type EvalMonad a = ExceptT Text (State (Store a))  


-- ExceptT Text (State (Store a)) (Expr a)
class (Num a, Eq a, Ord a) => Divisible a where
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
  
  Equals      :: (Eq a) =>  Expr a -> Expr a -> Expr Bool
  GreaterThan :: (Ord a) => Expr a -> Expr a -> Expr Bool
  LessThan    :: (Ord a) => Expr a -> Expr a -> Expr Bool

  Div         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Mul         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Sub         :: (Divisible n) => Expr n -> Expr n -> Expr n
  Add         :: (Divisible n) => Expr n -> Expr n -> Expr n
