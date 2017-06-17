{-# LANGUAGE RecordWildCards #-}

module Stack2Cabal.Constraints
    ( Constraint (..)
    , getConstraints
    , writeConstraintsBlock
    ) where

import Control.Exception (throwIO)
import Data.Text         (Text, pack, unpack)
import GHC.IO.Exception  (userError)
import System.Directory  (withCurrentDirectory)
import System.Process    (readProcess)

data Constraint = Constraint
    { cstName    :: !Text
    , cstVersion :: !Text
    } deriving Show

getConstraints :: FilePath -> IO [Constraint]
getConstraints dir =
    withCurrentDirectory dir $ do
        pairs <- (map words . lines) <$> readProcess "stack" ["list-dependencies"] ""
        mapM parse pairs
  where
    parse :: [String] -> IO Constraint
    parse [n, v] = return $ Constraint { cstName = pack n, cstVersion = pack v }
    parse x      = throwIO $ userError $ "can't parse constraint '" ++ show x ++ "'"

writeConstraintsBlock :: [Constraint] -> [String]
writeConstraintsBlock cs = zipWith f cs $ "constraints:   " : repeat "             , "
  where
    f :: Constraint -> String -> String
    f Constraint{..} s = s ++ unpack cstName ++ " == " ++ unpack cstVersion
