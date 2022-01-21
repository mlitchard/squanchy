module Examples.Fib where
import Types
import Eval
import Statements
import BasicPrelude
import qualified Data.Map.Lazy as M

zero :: Value
zero = Value $ NumberConst (0 :: Int)

one :: Value
one = Value $ NumberConst (1 :: Int)

booleanTest :: Value
booleanTest = Value $ (Not (Equals (NumberConst (1 :: Int)) (SquanchyVar "n")))

x :: Expr Int
x = SquanchyVar "x"

y :: Expr Int
y = SquanchyVar "y"

z :: Expr Int
z = SquanchyVar "z"

n :: Expr Int
n = SquanchyVar "n"

fibStore :: M.Map Text Value
fibStore = M.fromList [("n", Value $ NumberConst (25 :: Int))]

fib :: Prog
fib = Seq
  [ "x" := zero
  , "y" := one
  , While booleanTest $ Seq
      [ "z" := (Value (Add x y))
      , "x" := (Value y)
      , "y" := (Value z)
      , "n" := (Value ((Sub n (NumberConst 1 :: Expr Int) )))
      ]

 ]
