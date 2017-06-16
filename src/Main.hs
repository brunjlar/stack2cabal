module Main where

import Control.Monad      (forM_)
import System.Environment (getArgs)

import Stack2Yaml

main :: IO ()
main = do
  [dir] <- getArgs
  putStrLn $ "processing folder '" ++ dir ++ "'"
  yaml <- parseStackYaml dir
  let gs = gitPackages yaml
  forM_ gs print
