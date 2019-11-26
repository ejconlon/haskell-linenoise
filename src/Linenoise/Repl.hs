module Linenoise.Repl (
  replM
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (ByteString)
import qualified Linenoise.Unlift as Unlift

-- | Run a simple REPL.
replM
  :: MonadUnliftIO m
  => ByteString                      -- ^ Prompt
  -> (ByteString -> m ())            -- ^ Action
  -> (ByteString -> m [ByteString])  -- ^ Completion
  -> m ()
replM prompt action comp = loop where
  loop = do
    Unlift.setCompletion comp
    res <- Unlift.getInputLine prompt
    case res of
      Nothing -> pure ()
      Just line -> do
        _ <- action line
        Unlift.addHistory line
        loop

