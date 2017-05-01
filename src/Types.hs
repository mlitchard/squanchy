{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Types where

import BasicPrelude

data Expr a where
  BoolConst   :: Bool -> Expr Bool
  NumberConst :: Number a -> (Expr (Number a))
  Equals      :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr Bool
  GreaterThan :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr Bool
  LessThan    :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr Bool
  Div         :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr n
  Mul         :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr n
  Sub         :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr n
  Add         :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr n

class SquanchyNum a where
  data Number a :: *

  equals      :: Number a -> Number a -> Bool
  greaterThan :: Number a -> Number a -> Bool

instance SquanchyNum Integer where
  data Number Integer = NumInt Integer deriving (Eq,Show)

  equals (NumInt a) (NumInt b) = a == b
  greaterThan (NumInt a) (NumInt b) = a > b
