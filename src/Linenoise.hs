-- | Re-exports of all public entities.
module Linenoise
  ( ReplDirective (..)
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
import Linenoise.Unlift (addHistory, clearScreen, getInputLine, historyLoad, historySave, printKeycodes, setCompletion,
                         setMultiline, stifleHistory)
