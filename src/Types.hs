{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Types where

import BasicPrelude

data Expr a where
  BoolConst   :: Bool -> Expr Bool
  NumberConst :: Number a -> (Expr (Number a))

  Not         :: Expr Bool -> Expr Bool
  And         :: Expr Bool -> Expr Bool -> Expr Bool
  Or          :: Expr Bool -> Expr Bool -> Expr Bool
  Xor         :: Expr Bool -> Expr Bool -> Expr Bool

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


  equals :: Number a -> Number a -> Bool     
  greaterThan :: Number a -> Number a -> Bool
  lessThan    :: Number a -> Number a -> Bool

  divide :: Number a -> Number a -> a
  mul    :: Number a -> Number a -> a
  sub    :: Number a -> Number a -> a
  add    :: Number a -> Number a -> a

instance SquanchyNum Integer where
  data Number Integer = NumInt Integer deriving (Eq,Show)

  equals a b = a == b
  greaterThan (NumInt a) (NumInt b) = a > b
  lessThan (NumInt a) (NumInt b)    = a < b

  divide (NumInt x) (NumInt y) = x `div` y
  mul    (NumInt x) (NumInt y) = x * y
  sub    (NumInt x) (NumInt y) = x - y
  add    (NumInt x) (NumInt y) = x + y
