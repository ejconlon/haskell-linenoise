-- | Unlifted versions of all FFI methods.
module Linenoise.Unlift
  ( EditResult (..)
  , EditSession
  , InputResult (..)
  , addHistory
  , clearScreen
  , feedEditSession
  , freeEditSession
  , getInputLine
  , hideEditSession
  , historyLoad
  , historySave
  , printKeycodes
  , setCompletion
  , setMultiline
  , showEditSession
  , startEditSession
  , stopEditSession
  , stifleHistory
  , withEditSession
  , withHiddenEditSession
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Linenoise.FFI (EditResult (..), EditSession, InputResult (..))
import qualified Linenoise.FFI as FFI

-- | Add to current history.
addHistory :: (MonadIO m) => Text -> m ()
addHistory = liftIO . FFI.addHistory . encodeUtf8

-- | Clear the screen.
clearScreen :: (MonadIO m) => m ()
clearScreen = liftIO FFI.clearScreen

-- | Run the prompt, yielding a string.
getInputLine :: (MonadIO m) => Text -> m (InputResult Text)
getInputLine = liftIO . fmap (fmap decodeUtf8) . FFI.getInputLine . encodeUtf8

-- | Start a non-blocking edit session with the given prompt.
--
-- The returned session must eventually be passed to 'stopEditSession' and
-- 'freeEditSession'. Prefer 'withEditSession' for scoped use.
startEditSession :: (MonadIO m) => Text -> m EditSession
startEditSession = liftIO . FFI.startEditSession . encodeUtf8

-- | Stop editing and restore normal terminal mode.
--
-- This operation is idempotent for a given 'EditSession'.
stopEditSession :: (MonadIO m) => EditSession -> m ()
stopEditSession = liftIO . FFI.stopEditSession

-- | Free memory owned by an edit session.
--
-- Call this only after 'stopEditSession'. Prefer 'withEditSession'.
freeEditSession :: (MonadIO m) => EditSession -> m ()
freeEditSession = liftIO . FFI.freeEditSession

-- | Run an action with a managed non-blocking edit session.
--
-- The session is stopped and freed when the action exits, including on
-- exceptions.
withEditSession :: (MonadUnliftIO m) => Text -> (EditSession -> m a) -> m a
withEditSession prompt action =
  withRunInIO (\runInIO -> FFI.withEditSession (encodeUtf8 prompt) (runInIO . action))

-- | Feed one input event to a non-blocking edit session.
--
-- Returns 'MoreResult' until the user completes a line, interrupts, or sends
-- EOF.
feedEditSession :: (MonadIO m) => EditSession -> m (EditResult Text)
feedEditSession = liftIO . fmap (fmap decodeUtf8) . FFI.feedEditSession

-- | Hide the current prompt and edit buffer.
--
-- Use this before writing asynchronous output to the terminal.
hideEditSession :: (MonadIO m) => EditSession -> m ()
hideEditSession = liftIO . FFI.hideEditSession

-- | Redraw the current prompt and edit buffer.
--
-- Use this after writing asynchronous output to the terminal.
showEditSession :: (MonadIO m) => EditSession -> m ()
showEditSession = liftIO . FFI.showEditSession

-- | Run an action while the current prompt and edit buffer are hidden.
--
-- This is the primitive for prompt-safe asynchronous terminal output. Callers
-- that can write concurrently should still serialize access with a lock.
withHiddenEditSession :: (MonadUnliftIO m) => EditSession -> m a -> m a
withHiddenEditSession session action =
  withRunInIO (\runInIO -> FFI.withHiddenEditSession session (runInIO action))

-- | Load history from a file.
historyLoad :: (MonadIO m) => FilePath -> m ()
historyLoad = liftIO . FFI.historyLoad

-- | Save history to a file.
historySave :: (MonadIO m) => FilePath -> m ()
historySave = liftIO . FFI.historySave

-- | Print keycodes.
printKeycodes :: (MonadIO m) => m ()
printKeycodes = liftIO FFI.printKeycodes

-- | Set the current completion function.
setCompletion :: (MonadUnliftIO m) => (Text -> m [Text]) -> m ()
setCompletion f =
  let g = fmap (fmap encodeUtf8) . f . decodeUtf8
  in  withRunInIO (\runInIO -> FFI.setCompletion (runInIO . g))

-- | Enable/Disable multiline input.
setMultiline :: (MonadIO m) => Bool -> m ()
setMultiline = liftIO . FFI.setMultiline

-- | Limit the maximum history length.
stifleHistory :: (MonadIO m) => Int -> m ()
stifleHistory = liftIO . FFI.stifleHistory
