module Interpreter where

import BasicPrelude
import Control.Monad.State
import Control.Monad.Except

import Types (Store)
import Statements (exec, Prog)

runInterpreter :: Prog -> Store -> IO (Either Text (), Store)
runInterpreter prog store = runStateT (runExceptT (exec prog)) store

  
