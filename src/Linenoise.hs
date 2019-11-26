module Linenoise
  ( ReplT (..)
  , addHistory
  , byWord
  , getInputLine
  , replM
  , runReplT
  , setCompletion
  ) where

import Linenoise.Completion (byWord)
import Linenoise.Repl (ReplT (..), replM, runReplT)
import Linenoise.Unlift (addHistory, getInputLine, setCompletion)
