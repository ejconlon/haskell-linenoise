{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Console.Repl (
  Settings (..),
  defaultSettings,

  ReplT,
  runReplT,

  MonadRepl(..),

  replIO,
  replM,

  byWord
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

runReplT :: ReplT m a -> Settings -> m a
runReplT = runReaderT . unReplT

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
    lift (setCompletion (flip runReplT settings . f))

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
  -> (ByteString -> IO ())            -- ^ Action
  -> (ByteString -> IO [ByteString])  -- ^ Completion
  -> IO ()
replIO = replM

replM
  :: (MonadRepl m)
  => ByteString                      -- ^ Prompt
  -> (ByteString -> m ())            -- ^ Action
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

byWord :: Monad m => (ByteString -> m [ByteString]) -> (ByteString -> m [ByteString])
byWord f line = do
  let split = BSC.words line
  case split of
    [] -> f line
    [_] -> f line
    sp -> do
      let (x,xs) = (last sp, init sp)
      res <- f x
      case res of
        [] -> return [line]
        [y] ->
          return [BSC.unwords xs <> " " <> x <> trimComplete x y <> " "]
        ys ->
          return (map (complete x xs) ys)

complete :: ByteString -> [ByteString] -> ByteString -> ByteString
complete x xs y =
  BSC.unwords xs <> " " <> x <> trimComplete x y

trimComplete :: ByteString -> ByteString -> ByteString
trimComplete = BSC.drop . BSC.length
