{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import BasicPrelude

class (Num a, Eq a, Ord a) => Divisible a where
  divide :: a -> a -> a

instance Divisible Int where divide = div
instance Divisible Float where divide = (/)

data Expr a where
  BoolConst   ::  Bool -> Expr Bool
  NumberConst :: (Divisible a) => a -> Expr a
  SquanchyString :: Text -> Expr Text
  Not         :: Expr Bool -> Expr Bool
  And         :: Expr Bool -> Expr Bool -> Expr Bool
  Or          :: Expr Bool -> Expr Bool -> Expr Bool
  Xor         :: Expr Bool -> Expr Bool -> Expr Bool
  
  Equals      :: (Eq n) => Expr n -> Expr n -> Expr Bool
                 
  GreaterThan :: (Ord n) => Expr n -> Expr n -> Expr Bool
                 
  LessThan    :: (Ord n) => Expr n -> Expr n -> Expr Bool

  Div         :: (Divisible n) => Expr n -> Expr n -> Expr n
                 
  Mul         :: (Divisible n) => Expr n -> Expr n -> Expr n
                 
  Sub         :: (Divisible n) => Expr n -> Expr n -> Expr n
                 
  Add         :: (Divisible n) => Expr n -> Expr n -> Expr n
