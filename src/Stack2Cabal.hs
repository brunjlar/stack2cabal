{-# LANGUAGE RecordWildCards #-}

module Stack2Cabal
    ( parseConfig
    , configCabal
    ) where

import Control.Monad           (when, forM_)
import Data.Maybe              (mapMaybe)
import System.Directory        (doesPathExist)
import System.FilePath         ((</>), (<.>))
import System.IO               (withFile, IOMode (WriteMode), hPutStrLn)

import Stack2Cabal.Compiler
import Stack2Cabal.Config
import Stack2Cabal.Constraints
import Stack2Cabal.Git
import Stack2Cabal.Packages
import Stack2Cabal.Util

configCabal :: Config -> IO ()
configCabal cnf@Config{..} =
    withLog ("creating cabal config for project folder '" ++ cnfProjectDir ++ "'") $ do
    packages <- parseStackYaml cnfProjectDir
    writeCabalProject cnf packages
    handleGitDependencies cnf packages

writeCabalProject :: Config -> [Package] -> IO ()
writeCabalProject Config{..} packages = do
    let file = cnfProjectDir </> "cabal" <.> "project"
    b <- doesPathExist file
    case (b, cnfCabalProject) of
        (True,  Force) -> wcp file
        (True,  Yes)   -> putLogLn "cabal.project already exists"
        (False, Force) -> wcp file
        (False, Yes)   -> wcp file
        (_,     No)    -> return ()
  where
    wcp :: FilePath -> IO ()
    wcp file = withLog "writing cabal.project" $
        withFile file WriteMode $ \h -> do
            let ps = writePackagesBlock gitFolder packages -- <$> parseStackYaml cnfProjectDir
            cs <- writeCompilerBlock <$> getCompiler cnfProjectDir
            ds <- writeConstraintsBlock <$> getConstraints cnfProjectDir
            mapM_ (hPutStrLn h) $ ps ++ cs ++ ds

handleGitDependencies :: Config -> [Package] -> IO ()
handleGitDependencies Config{..} packages = 
    when cnfGitDependencies $ do
        let dir = cnfProjectDir </> gitFolder
            gs  = mapMaybe filterGit packages 
        forM_ gs $ \git ->
            cloneAndCheckoutGit dir git
  where
    filterGit :: Package -> Maybe Git
    filterGit (GitPackage git) = Just git
    filterGit (LocalPackage _) = Nothing

gitFolder :: FilePath
gitFolder = ".stack2cabal"
