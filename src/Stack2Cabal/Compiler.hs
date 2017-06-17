module Stack2Cabal.Compiler
    ( getCompiler
    ) where

import System.Directory (withCurrentDirectory)
import System.Process   (readProcess)

getCompiler :: FilePath -> IO String
getCompiler dir =
    withCurrentDirectory dir $ do
        (head . lines) <$> readProcess "stack" ["path", "--compiler-exe"] ""
