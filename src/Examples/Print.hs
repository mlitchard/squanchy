module Examples.Print where
import Types
import Statements
import BasicPrelude
import qualified Data.Map.Lazy as M

zero :: Value
zero = Value $ NumberConst (0 :: Int)

one :: Value
one = Value $ NumberConst (1 :: Int)

textStore :: M.Map Text Value
textStore = M.fromList [("n", Value $ SquanchyString ("25" :: Text))]

textTest  :: Prog
textTest = Seq [Print (Value $ (SquanchyVar "n" :: Expr Text))]

boolStore :: M.Map Text Value
boolStore = M.fromList [("n", Value $ BoolConst (True :: Bool))]

boolTest :: Prog
boolTest = Seq [Print (Value $ (SquanchyVar "n" :: Expr Bool))]


intStore :: M.Map Text Value
intStore = M.fromList [("n", Value $ NumberConst (1 :: Int))]

intTest :: Prog
intTest = Seq [Print (Value $ (SquanchyVar "n" :: Expr Int))]

floatStore :: M.Map Text Value
floatStore = M.fromList [("n", Value $ NumberConst (1 :: Float))]

floatTest :: Prog
floatTest = Seq [Print (Value $ (SquanchyVar "n" :: Expr Float))]
