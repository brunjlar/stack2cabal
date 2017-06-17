module Stack2Cabal.Compiler
    ( getCompiler
    , writeCompilerBlock
    ) where

import Data.Text        (Text, pack, unpack)
import System.Directory (withCurrentDirectory)
import System.Process   (readProcess)

newtype Compiler = Compiler Text
    deriving Show

getCompiler :: FilePath -> IO Compiler
getCompiler dir =
    withCurrentDirectory dir $ do
        (Compiler . pack . head . lines) <$> readProcess "stack" ["path", "--compiler-exe"] ""

writeCompilerBlock :: Compiler -> [String]
writeCompilerBlock (Compiler c) = ["with-compiler: " ++ unpack c]
