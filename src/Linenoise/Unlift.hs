-- | Unlifted versions of all FFI methods.
module Linenoise.Unlift
  ( InputResult (..)
  , addHistory
  , clearScreen
  , getInputLine
  , historyLoad
  , historySave
  , printKeycodes
  , setCompletion
  , setMultiline
  , stifleHistory
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Linenoise.FFI (InputResult (..))
import qualified Linenoise.FFI as FFI

-- | Add to current history.
addHistory :: MonadIO m => ByteString -> m ()
addHistory = liftIO . FFI.addHistory

-- | Clear the screen.
clearScreen :: MonadIO m => m ()
clearScreen = liftIO FFI.clearScreen

-- | Run the prompt, yielding a string.
getInputLine :: MonadIO m => ByteString -> m InputResult
getInputLine = liftIO . FFI.getInputLine

-- | Load history from a file.
historyLoad :: MonadIO m => FilePath -> m ()
historyLoad = liftIO . FFI.historyLoad

-- | Save history to a file.
historySave :: MonadIO m => FilePath -> m ()
historySave = liftIO . FFI.historySave

-- | Print keycodes.
printKeycodes :: MonadIO m => m ()
printKeycodes = liftIO FFI.printKeycodes

-- | Set the current completion function.
setCompletion :: MonadUnliftIO m => (ByteString -> m [ByteString]) -> m ()
setCompletion f = withRunInIO (\runInIO -> FFI.setCompletion (runInIO . f))

-- | Enable/Disable multiline input.
setMultiline :: MonadIO m => Bool -> m ()
setMultiline = liftIO . FFI.setMultiline

-- | Limit the maximum history length.
stifleHistory :: MonadIO m => Int -> m ()
stifleHistory = liftIO . FFI.stifleHistory
