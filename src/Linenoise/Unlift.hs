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
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Linenoise.FFI (InputResult (..))
import qualified Linenoise.FFI as FFI

-- | Add to current history.
addHistory :: MonadIO m => Text -> m ()
addHistory = liftIO . FFI.addHistory . encodeUtf8

-- | Clear the screen.
clearScreen :: MonadIO m => m ()
clearScreen = liftIO FFI.clearScreen

-- | Run the prompt, yielding a string.
getInputLine :: MonadIO m => Text -> m (InputResult Text)
getInputLine = liftIO . fmap (fmap decodeUtf8) . FFI.getInputLine . encodeUtf8

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
setCompletion :: MonadUnliftIO m => (Text -> m [Text]) -> m ()
setCompletion f =
  let g = fmap (fmap encodeUtf8) . f . decodeUtf8
  in  withRunInIO (\runInIO -> FFI.setCompletion (runInIO . g))

-- | Enable/Disable multiline input.
setMultiline :: MonadIO m => Bool -> m ()
setMultiline = liftIO . FFI.setMultiline

-- | Limit the maximum history length.
stifleHistory :: MonadIO m => Int -> m ()
stifleHistory = liftIO . FFI.stifleHistory
