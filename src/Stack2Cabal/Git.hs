{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack2Cabal.Git
    ( Git (..)
    , gitName
    , gitNameIO
    , cloneAndCheckoutGit
    ) where

import           Control.Applicative
import           Control.Exception    (throwIO)
import qualified Data.Attoparsec.Text as A
import           Data.Text            (Text, pack, unpack)
import           GHC.IO.Exception     (userError)
import           System.FilePath      ((</>))
import           System.Directory     (withCurrentDirectory, doesDirectoryExist)
import           System.Process       (callProcess)

import           Stack2Cabal.Util     (withLog, putLogLn)

data Git = Git 
    { gitUrl    :: !Text
    , gitCommit :: !Text
    } deriving (Eq, Ord, Show)

gitName :: Git -> Either String Text
gitName = A.parseOnly gitNameP . gitUrl
  where
    gitNameP :: A.Parser Text
    gitNameP = pack <$> (  A.string "https://github.com/" 
                        *> some (A.notChar '/') 
                        *> A.char '/' 
                        *> some (A.notChar '.') 
                        <* optional (A.string ".git")
                        <* A.endOfInput
                        )

gitNameIO :: Git -> IO Text
gitNameIO git = case gitName git of
    Left e  -> throwIO $ userError e
    Right n -> return n

cloneAndCheckoutGit :: FilePath -> Git -> IO ()
cloneAndCheckoutGit dir git@Git{..} =
    withLog ("handling git " ++ show git) $ do
        name <- unpack <$> gitNameIO git
        let url = unpack gitUrl
            dir'   = dir </> name
            commit = unpack gitCommit
        withCurrentDirectory dir $ do
            b <- doesDirectoryExist name
            if b then putLogLn $ "git repository '" ++ name ++ "' has already been cloned"
                 else withLog ("cloning git repository '" ++ name ++ "' from '" ++ url ++ "'") $
                     callProcess "git" ["clone", url]
        withCurrentDirectory dir' $
            withLog ("checking out commit '" ++ commit ++ "'") $
                callProcess "git" ["checkout", commit]
