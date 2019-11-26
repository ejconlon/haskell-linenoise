{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State.Strict (MonadState, StateT, get, modify, put, runStateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Console.Repl

newtype History = History { unHistory :: [ByteString] }

newtype Repl a = Repl { unRepl :: ReaderT (IORef History) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadState History Repl where
  get = Repl (ReaderT readIORef)
  put = Repl . ReaderT . flip writeIORef

runRepl :: Repl a -> History -> IO (a, History)
runRepl r h = do
  ref <- newIORef h
  res <- runReaderT (unRepl r) ref
  final <- readIORef ref
  pure (res, final)

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
