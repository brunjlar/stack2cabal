{-# LANGUAGE OverloadedStrings #-}
module Stack2Cabal.Packages
    ( Package (..)
    , parseStackYaml
    , writePackagesBlock
    ) where

import Data.Maybe         (mapMaybe)
import Data.Text          (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml.Parser   (YamlValue (..), readYamlFile)
import System.FilePath    ((</>), (<.>))

import Stack2Cabal.Git    (Git (..), gitName)

data Package =
      LocalPackage !Text
    | GitPackage !Git
    deriving Show

parseStackYaml :: FilePath -> IO [Package]
parseStackYaml dir = parsePackages <$> (readYamlFile $ dir </> "stack" <.> "yaml")
  where
    parsePackages :: YamlValue -> [Package]
    parsePackages (Mapping m _) = case lookup "packages" m of
        Just (Sequence ps _) -> mapMaybe parsePackage ps
        _                    -> []
    parsePackages _             = []

    parsePackage :: YamlValue -> Maybe Package
    parsePackage (Mapping (("location", Mapping p _) : _) _) =
        case (lookup "git" p, lookup "commit" p) of
            (Just (Scalar g _ _ _), Just (Scalar c _ _ _)) -> Just $ GitPackage $ Git { gitUrl = decodeUtf8 g, gitCommit = decodeUtf8 c }
            _                                              -> Nothing
    parsePackage (Scalar s _ _ _)                            = Just $ LocalPackage $ decodeUtf8 s
    parsePackage _                                           = Nothing

writePackagesBlock :: FilePath -> [Package] -> [String]
writePackagesBlock gitDir ps = filter (not . null) $ zipWith f ps $ "packages: " : repeat "          "
  where
    f :: Package -> String -> String
    f p s = s ++ g p

    g :: Package -> String
    g (GitPackage git) = case gitName git of
        Left _  -> ""
        Right n -> gitDir ++ "/" ++ unpack n ++ "/"
    g (LocalPackage l) = unpack l ++ "/"
