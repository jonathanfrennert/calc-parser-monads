module Main where

import NanoParsec
import Control.Monad
import System.IO

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  str <- getLine
  print $ eval $ run str
