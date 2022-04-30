module Main where

import Lib (parseLog)
import LogParser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  parsed <- fmap (map parseLog . lines) (readFile $ head args)

  print parsed

  return ()
