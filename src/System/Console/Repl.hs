{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module System.Console.Repl (
  ReplT,
  runRepl,

  Settings (..),
  defaultSettings,

  replIO,
  replM,

  MonadRepl(..),
) where

import qualified System.Console.FFI as FFI
import Data.ByteString (ByteString)
-- TODO(econlon) Support UTF8
import qualified Data.ByteString.Char8 as BSC

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Catch

data Settings = Settings
  { historyFile :: Maybe FilePath
  }

defaultSettings :: Settings
defaultSettings = Settings Nothing

newtype ReplT m a =
  ReplT { unReplT :: ReaderT Settings m a }
  deriving (Functor, Monad, Applicative, MonadIO, MonadReader Settings, MonadFix, MonadTrans, MonadThrow, MonadCatch)

runRepl :: ReplT m a -> Settings -> m a
runRepl m s = runReaderT (unReplT m) s

class MonadCatch m => MonadRepl m where
  getInputLine  :: ByteString -> m (Maybe ByteString)
  outputStr     :: ByteString -> m ()
  outputStrLn   :: ByteString -> m ()
  addHistory    :: ByteString -> m ()
  setCompletion :: (ByteString -> m [ByteString]) -> m ()

instance MonadRepl IO where
  getInputLine  = FFI.getInputLine
  outputStr     = BSC.putStr
  outputStrLn   = BSC.putStrLn
  addHistory    = FFI.addHistory
  setCompletion = FFI.setCompletion

instance MonadRepl m => MonadRepl (ReplT m) where
  getInputLine  = lift . getInputLine
  outputStr     = lift . outputStr
  outputStrLn   = lift . outputStrLn
  addHistory    = lift . addHistory

  setCompletion f = do
    settings <- ask
    lift (setCompletion (flip runRepl settings . f))

instance MonadState s m => MonadState s (ReplT m) where
  get = lift get
  put = lift . put

instance MonadRepl m => MonadRepl (StateT s m) where
  getInputLine  = lift . getInputLine
  outputStr     = lift . outputStr
  outputStrLn   = lift . outputStrLn
  addHistory    = lift . addHistory
  setCompletion f = do
    st <- get
    lift (setCompletion (flip evalStateT st . f))

-- | Simple REPL embedded in IO.
replIO
  :: ByteString                       -- ^ Prompt
  -> (ByteString -> IO a)             -- ^ Action
  -> (ByteString -> IO [ByteString])  -- ^ Completion
  -> IO ()
replIO = replM

replM
  :: (MonadRepl m)
  => ByteString                      -- ^ Prompt
  -> (ByteString -> m a)             -- ^ Action
  -> (ByteString -> m [ByteString])  -- ^ Completion
  -> m ()
replM prompt action comp = do
  setCompletion comp
  res <- getInputLine prompt
  case res of
    Nothing   -> return ()
    Just line -> do
      _ <- action line
      addHistory line
      replM prompt action comp
