module Stack2Cabal.Util
    ( putLogLn
    , withLog
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import System.IO              (stderr, hPutStrLn)

putLogLn :: MonadIO m => String -> m ()
putLogLn = liftIO . hPutStrLn stderr

withLog :: MonadIO m => String -> m a -> m a
withLog s m = do
    putLogLn s
    x <- m
    putLogLn $ "done: " ++ s
    return x
