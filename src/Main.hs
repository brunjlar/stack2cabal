module Main where

import Stack2Cabal

main :: IO ()
main = do
  config <- parseConfig
  print config
  {-
  [dir] <- getArgs
  withLog ("processing folder '" ++ dir ++ "'") $
    writeCabalProject dir
    -}
