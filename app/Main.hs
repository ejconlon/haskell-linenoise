{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Linenoise

type History = [ByteString]

type Repl a = ReplT () History IO a

runRepl :: Repl a -> History -> IO (a, History)
runRepl n = runReplT n ()

completer :: ByteString -> Repl [ByteString]
completer line = filter (BSC.isPrefixOf line) <$> get

action :: ByteString -> Repl ReplDirective
action x = do
  modify (x:)
  liftIO (BSC.putStrLn x)
  pure ReplContinue

repl :: Repl ()
repl = replM ReplContinue ">>> " action (byWord completer)

main :: IO ()
main = void (runRepl repl [])
