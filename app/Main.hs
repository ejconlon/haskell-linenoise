{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Linenoise

type History = [Text]

type Repl a = ReplT () History IO a

runRepl :: Repl a -> History -> IO (a, History)
runRepl n = runReplT n ()

completer :: Text -> Repl [Text]
completer line = filter (Text.isPrefixOf line) <$> get

action :: Text -> Repl ReplDirective
action x = do
  modify (x :)
  liftIO (TIO.putStrLn x)
  pure ReplContinue

repl :: Repl ()
repl = replM ReplContinue ">>> " action (byWord completer)

main :: IO ()
main = void (runRepl repl [])
