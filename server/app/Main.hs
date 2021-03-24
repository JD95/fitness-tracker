module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [host, port, user, pass, db] <- getArgs
  app (Config host (read port) user pass db)
