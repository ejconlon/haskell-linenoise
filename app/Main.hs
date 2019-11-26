{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import System.Console.Repl

type Repl = ReplT (StateT [ByteString] IO)

completer :: ByteString -> Repl [ByteString]
completer line = filter (BSC.isPrefixOf line) <$> get

action :: ByteString -> Repl ()
action x = do
  modify (x:)
  liftIO (BSC.putStrLn x)

repl :: Repl ()
repl = replM ">>> " action (byWord completer)

main :: IO ()
main = evalStateT (runReplT repl defaultSettings) []
