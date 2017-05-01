module Types where

import BasicPrelude

data Expr a where
  BoolConst   :: Bool -> Expr Bool
  NumberConst :: Number a -> (Expr (Number a))
  Equals      :: (SquanchyNum n) => Expr (Number n)
                                 -> Expr (Number n)
                                 -> Expr Bool

class SquanchyNum a where
  data Number a :: *

  equals :: Number a -> Number a -> Bool

instance SquanchyNum Integer where
  data Number Integer = NumInt Integer deriving (Eq,Show)

  equals (NumInt a) (NumInt b) = a == b
