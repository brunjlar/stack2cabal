{-# LANGUAGE OverloadedStrings #-}

module Stack2Cabal.StackYaml
    ( parseStackYaml
    , gitPackages
    ) where

import Data.Maybe         (mapMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml.Parser   (YamlValue (..), readYamlFile)
import System.FilePath    ((</>), (<.>))

import Stack2Cabal.Git    (Git (..))

parseStackYaml :: FilePath -> IO YamlValue
parseStackYaml dir = readYamlFile $ dir </> "stack" <.> "yaml"

gitPackages :: YamlValue -> [Git]
gitPackages (Mapping m _) = case lookup "packages" m of
    Just (Sequence ps _) -> mapMaybe f [p | Mapping (("location",  Mapping p _) : _) _ <- ps]
    _                    -> []
  where
    --f :: YamlValue -> 
    f m' = case (lookup "git" m', lookup "commit" m') of
        (Just (Scalar g _ _ _), Just (Scalar c _ _ _)) -> Just $ Git { gitUrl = decodeUtf8 g, gitCommit = decodeUtf8 c }
        _                                              -> Nothing
gitPackages _           = []
