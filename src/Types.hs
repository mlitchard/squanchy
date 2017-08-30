{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import BasicPrelude

data Expr a where
  BoolConst   ::  Bool -> Expr Bool
  NumberConst :: (Num a) => a -> Expr a
  SquanchyString :: Text -> Expr Text
  SquanchyVar :: Text -> Expr a -> Expr (Text, a) 
  Not         :: Expr Bool -> Expr Bool
  And         :: Expr Bool -> Expr Bool -> Expr Bool
  Or          :: Expr Bool -> Expr Bool -> Expr Bool
  Xor         :: Expr Bool -> Expr Bool -> Expr Bool
  
  Equals      :: (Eq n) => Expr n -> Expr n -> Expr Bool
                 
  GreaterThan :: (Ord n) => Expr n
                         -> Expr n
                         -> Expr Bool
                 
  LessThan    :: (Ord n) => Expr n
                         -> Expr n
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
data family Number :: * -> *
data instance Number Integer = NumInt Integer deriving (Eq, Ord, Show)
data instance Number Float   = NumFloat Float deriving (Eq, Ord, Show)

class (Eq a, Ord a, Num a) => SquanchyNum a where

  divide :: Number a -> Number a -> a
  mul    :: Number a -> Number a -> a
  sub    :: Number a -> Number a -> a
  add    :: Number a -> Number a -> a
  
instance SquanchyNum Integer where

  divide (NumInt x) (NumInt y) = x `div` y
  mul    (NumInt x) (NumInt y) = x * y
  sub    (NumInt x) (NumInt y) = x - y
  add    (NumInt x) (NumInt y) = x + y
  
instance SquanchyNum Float where

  divide (NumFloat x) (NumFloat y) = x / y
  mul    (NumFloat x) (NumFloat y) = x * y
  sub    (NumFloat x) (NumFloat y) = x - y
  add    (NumFloat x) (NumFloat y) = x + y
