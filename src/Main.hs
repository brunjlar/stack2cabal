module Main where

import System.Environment (getArgs)

import Stack2Cabal

main :: IO ()
main = do
  [dir] <- getArgs
  withLog ("processing folder '" ++ dir ++ "'") $
    writeCabalProject dir
