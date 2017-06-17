module Main where

import Stack2Cabal

main :: IO ()
main = parseConfig >>= configCabal
