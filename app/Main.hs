module Main where

import Lib
import System.Environment
import Text.Read
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    (nstr:[]) | Just n <- readMaybe nstr, n > 0 -> chartSteps n
    _ -> do
      putStrLn "Usage: fractal-exe <n>"
      putStrLn "  n must > 0"
      exitFailure
