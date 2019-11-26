module Linenoise
  ( getInputLine
  , addHistory
  , setCompletion
  , replM
  , byWord
  ) where

import Linenoise.Completion (byWord)
import Linenoise.Repl (replM)
import Linenoise.Unlift (addHistory, getInputLine, setCompletion)
