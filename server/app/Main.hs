module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [host, port, user, pass, db] <- getArgs
  app (Env host (read port) user pass db)
