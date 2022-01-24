module Statements where

import qualified Data.Map.Lazy as M
import BasicPrelude
import Control.Monad.State
import Data.Text
import Data.Typeable
import Text.Read hiding (get)

import Eval
import Types

infix 1 :=

type Prog = Stmt

data Stmt 
 = Var := Value
 | Get Var
 | Print Value
 | While Value Stmt
 | Seq [Stmt]


printText :: Text -> EvalMonad ()
printText var = do
  store <- get
  case (M.lookup var store) of
    Nothing -> error "variable doesn't exist"
    Just res -> do
                  case (castText res) of
                    Just res' -> do
                                   res'' <- eval res'
                                   case (castText res'') of
                                     Just (SquanchyString txt) -> liftIO
                                                                    $ putStrLn
                                                                        txt
                                     _ -> error "castText failed"
                    _ -> error "castText failed" 

printBool :: Text -> EvalMonad ()
printBool var = do
  store <- get
  case (M.lookup var store) of
    Nothing -> error "variable doesn't exist"
    Just res -> do
                  case (castBool res) of
                    Just res' -> do
                                   res'' <- eval res'
                                   case (castBool res'') of
                                     Just (BoolConst b) -> liftIO $ putStrLn
                                                             $ pack $ show b
                                     _ -> error "castBool failed"
                    _ -> error "castBool failed"

printInt :: Text -> EvalMonad ()
printInt var = do
  store <- get
  case (M.lookup var store) of
    Nothing -> error "variable doesn't exist"
    Just res -> do
                  case (castInt res) of
                    Just res' -> do
                                   res'' <- eval res'
                                   case (castInt res'') of
                                     Just (NumberConst b) -> liftIO $ putStrLn
                                                             $ pack $ show b
                                     _ -> error "castInt failed"
                    _ -> error "castInt failed"


printFloat :: Text -> EvalMonad ()
printFloat var = do
  store <- get
  case (M.lookup var store) of
    Nothing -> error "variable doesn't exist"
    Just res -> do
                  case (castFloat res) of
                    Just res' -> do
                                   res'' <- eval res'
                                   case (castFloat res'') of
                                     Just (NumberConst b) -> liftIO $ putStrLn
                                                             $ pack $ show b
                                     _ -> error "castInt failed"
                    _ -> error "castInt failed"

printVal :: Value -> EvalMonad ()
printVal val = do
  case (castText val) of
    Just (SquanchyVar var) -> printText var
    _ -> case (castInt val) of
      Just (SquanchyVar var) -> printInt var
      _  -> case (castFloat val) of
        Just (SquanchyVar flt)  -> printFloat flt
        _  -> case (castBool val) of
          Just (SquanchyVar b) -> printBool b
          _ -> error "Value could not be reduced"


doStore :: Text -> Value -> EvalMonad ()
doStore var val = do
  case (castInt val) of
    Just intExpr -> do
                      res <- eval intExpr
                      case (castInt res) of
                        Just (NumberConst _) -> modify $ M.insert var res
                        _ -> error "illegal value"
    Nothing      -> do
      case (castFloat val) of
        Just floatExpr -> do
                            res <- eval floatExpr
                            case (castFloat res) of     
                              Just (NumberConst _) -> modify $ M.insert var res
                              _ -> error "illegal value"
        Nothing -> case (castBool val) of
          (Just boolExpr) -> do
                               res <- eval boolExpr
                               case (castBool res) of
                                 Just (BoolConst _) -> modify $ M.insert var res
                                 _ -> error "illegal value"
          Nothing         -> do
            case (castText val) of
              (Just textExpr) -> do
                                   res <- eval textExpr
                                   case (castText res) of
                                     Just (SquanchyString _) -> 
                                       modify $ M.insert var res 

                                     _  -> error "illegal value"
              Nothing -> error "illegal value"                        
                                   
          
readBool :: String -> Maybe Bool
readBool str = readMaybe str

readInt :: String -> Maybe Int
readInt str = readMaybe str

readFloat :: String -> Maybe Float
readFloat str = readMaybe str

readText :: String -> Maybe Text
readText str = do
  case (readMaybe str :: Maybe String) of
    Just txt -> return $ pack txt
    Nothing  -> Nothing 
 
input :: IO Value
input = do
  var <- unpack <$> getLine
  case (readBool var) of
    Just b  -> return $ Value $ BoolConst b 
    Nothing -> case (readInt var) of
                 Just i  -> return $ Value $ NumberConst i
                 Nothing -> case (readFloat var) of
                              Just flt -> return 
                                            $ Value $ NumberConst flt
                              Nothing  -> return 
                                            $ Value $ SquanchyString (pack var)

exec :: Prog -> EvalMonad ()
exec (var := val)  = doStore var val
exec (Print val) = printVal val
                     
exec (Get var) = do
                   val <- liftIO $ input
                   doStore var val
exec (While e s)   = do
  let expr :: (Maybe (Expr Bool))
      expr = toExpr e 
  case expr of
    Just res -> do
                  res' :: Value <- eval res
                  let expr' :: (Maybe (Expr Bool))
                      expr' = castBool res'
                     
                  case expr' of
                    Just (BoolConst b) -> when b
                                            $ exec (Seq [s, While e s])
                    p                  -> error $ reductionError p
    _      -> error whileError 
exec (Seq [])      = return ()
exec (Seq (s:ss))  = do
                       exec s
                       exec (Seq ss)

whileError :: String
whileError = "While needs a boolean to evaluate"

reductionError :: Maybe (Expr Bool) -> String
reductionError Nothing = "Not a bool"
reductionError (Just (Sub _ _)) = "it never reduced the sub"
reductionError x'' = 
  "While couldn't reduce the boolean value for some reason " 
    ++ (show $ typeOf x'')

