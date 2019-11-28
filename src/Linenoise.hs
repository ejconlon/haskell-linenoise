-- | Re-exports of all public entities.
module Linenoise
  ( InputResult (..)
  , ReplDirective (..)
  , ReplT (..)
  , addHistory
  , byWord
  , clearScreen
  , getInputLine
  , historyLoad
  , historySave
  , printKeycodes
  , replM
  , runReplT
  , setCompletion
  , setMultiline
  , stifleHistory
  ) where

import Linenoise.Completion (byWord)
import Linenoise.Repl (ReplDirective (..), ReplT (..), replM, runReplT)
import Linenoise.Unlift (InputResult (..), addHistory, clearScreen, getInputLine, historyLoad, historySave,
                         printKeycodes, setCompletion, setMultiline, stifleHistory)
