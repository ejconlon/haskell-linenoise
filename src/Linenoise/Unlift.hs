module Linenoise.Unlift (
  getInputLine,
  addHistory,
  setCompletion,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import qualified Linenoise.FFI as FFI

-- | Run the prompt, yielding a string.
getInputLine :: MonadIO m => ByteString -> m (Maybe ByteString)
getInputLine = liftIO . FFI.getInputLine

-- | Add to current history.
addHistory :: MonadIO m => ByteString -> m ()
addHistory = liftIO . FFI.addHistory

-- | Set the current completion function
setCompletion :: MonadUnliftIO m => (ByteString -> m [ByteString]) -> m ()
setCompletion f = withRunInIO (\runInIO -> FFI.setCompletion (runInIO . f))
