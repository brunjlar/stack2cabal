module Stack2Cabal
    ( module Stack2Cabal.Compiler
    , module Stack2Cabal.Constraints
    , module Stack2Cabal.Git
    , module Stack2Cabal.Packages
    , module Stack2Cabal.Util
    , writeCabalProject
    ) where

import System.Directory        (doesPathExist)
import System.FilePath         ((</>), (<.>))
import System.IO               (withFile, IOMode (WriteMode), hPutStrLn)

import Stack2Cabal.Compiler
import Stack2Cabal.Constraints
import Stack2Cabal.Git
import Stack2Cabal.Packages
import Stack2Cabal.Util

writeCabalProject :: FilePath -> IO ()
writeCabalProject dir = do
    let file = dir </> "cabal" <.> "project"
    b <- doesPathExist file
    if b then putLogLn "cabal project file already exists"
         else withLog "writing cabal project file" $ 
                withFile file WriteMode $ \h -> do
                    ps <- writePackagesBlock "git-packages" <$> parseStackYaml dir
                    cs <- writeCompilerBlock <$> getCompiler dir
                    ds <- writeConstraintsBlock <$> getConstraints dir
                    mapM_ (hPutStrLn h) $ ps ++ cs ++ ds
