module Stack2Cabal.Compiler
    ( getCompiler
    ) where

import Data.Text        (Text, pack)
import System.Directory (withCurrentDirectory)
import System.Process   (readProcess)

getCompiler :: FilePath -> IO Text
getCompiler dir =
    withCurrentDirectory dir $ do
        (pack . head . lines) <$> readProcess "stack" ["path", "--compiler-exe"] ""
