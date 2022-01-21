module Main where

import BasicPrelude
import Interpreter
import Types
import Examples.Fib

import Data.Text
import qualified Data.Map.Lazy as M 

main :: IO ()
main = do
  finalStore <- snd <$> runInterpreter fib fibStore
  let res = join $ showValue <$> M.lookup "z" finalStore
  putStrLn ("The 25th fibonacci value is " <> (pack $ show $ res))
