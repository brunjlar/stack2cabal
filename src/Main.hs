module Main where

import Control.Monad      (forM_)
import System.Directory   (withCurrentDirectory, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath    ((</>))

import Stack2Cabal

main :: IO ()
main = do
  [dir] <- getArgs
  withCurrentDirectory dir $
      withLog ("processing folder '" ++ dir ++ "'") $ do
          yaml <- parseStackYaml dir
          let gitDir = dir </> "git-packages"
          createDirectoryIfMissing False gitDir
          forM_ (gitPackages yaml) $ \g -> do
            cloneAndCheckoutGit (dir </> "git-packages")  g
