{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Linenoise

newtype History = History { unHistory :: [ByteString] }

type Repl a = ReplT () History IO a

runRepl :: Repl a -> History -> IO (a, History)
runRepl n = runReplT n ()

completer :: ByteString -> Repl [ByteString]
completer line = filter (BSC.isPrefixOf line) . unHistory <$> get

action :: ByteString -> Repl ()
action x = do
  modify (History . (x:) . unHistory)
  liftIO (BSC.putStrLn x)

repl :: Repl ()
repl = replM ">>> " action (byWord completer)

main :: IO ()
main = void (runRepl repl (History []))
