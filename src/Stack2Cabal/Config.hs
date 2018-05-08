module Stack2Cabal.Config
    ( Config (..)
    , CreateCabalProject (..)
    , parseConfig
    ) where

import Data.Monoid         ((<>))
import Options.Applicative

data Config = Config
    { cnfCabalProject    :: !CreateCabalProject
    , cnfGitDependencies :: !Bool
    , cnfProjectDir      :: !FilePath
    } deriving Show

data CreateCabalProject =
      Force
    | Yes
    | No
    deriving (Show, Read)

configP :: Parser Config
configP = Config <$> option auto
                     (  long "cabal-project"
                     <> short 'c'
                     <> metavar "CABALPROJECT"
                     <> value Yes
                     <> showDefault
                     <> help "create cabal.project file - possible values are Force, Yes and No"
                     )
                 <*> (not <$> switch
                     (  long "no-git-dependencies"
                     <> short 'n'
                     <> help "do not download git dependencies"
                     ))
                 <*> argument str
                     (  metavar "PROJECTFOLDER"
                     )

configInfo :: ParserInfo Config
configInfo = info (configP <**> helper)
    (  fullDesc
    <> progDesc "Creates a configuration suitable for cabal new-build from the stack project in PROJECTFOLDER."
    <> header "stack2cabal - configures a stack project for cabal new-build"
    )

parseConfig :: IO Config
parseConfig = execParser configInfo
