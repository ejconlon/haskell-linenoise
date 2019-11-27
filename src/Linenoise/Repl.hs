{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Helper functions and transformer to write your own REPLs.
module Linenoise.Repl
  ( ReplDirective (..)
  , ReplT (..)
  , replM
  , runReplT
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Zip (MonadZip)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Linenoise.Unlift as Unlift

-- | Basic monad transformer with mutable state that can be used with all "Linenoise.Unlift" functions.
--   You do not have to use this, but it's here to cover most of what you would need without having
--   to roll your own newtype.
newtype ReplT r s m a = ReplT { unReplT :: ReaderT r (ReaderT (IORef s) m) a }
  deriving (Functor, Applicative, Monad, MonadIO,
            Alternative, MonadPlus, MonadFix, MonadZip,
            MonadFail, MonadThrow, MonadCatch,
            MonadReader r)

askRef :: Applicative m => ReplT r s m (IORef s)
askRef = ReplT (ReaderT (const (ReaderT pure)))

refReplT :: ReplT r s m a -> r -> IORef s -> m a
refReplT n r = runReaderT (runReaderT (unReplT n) r)

instance MonadTrans (ReplT r s) where
  lift = ReplT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (ReplT r s m) where
  askUnliftIO = do
    UnliftIO run <- lift askUnliftIO
    r <- ask
    ref <- askRef
    pure (UnliftIO (\n -> run (refReplT n r ref)))

instance MonadIO m => MonadState s (ReplT r s m) where
  get = ReplT (ReaderT (const (ReaderT (liftIO . readIORef))))
  put s = ReplT (ReaderT (const (ReaderT (\ref -> liftIO (writeIORef ref s)))))

-- | Run a ReplT.
runReplT :: MonadIO m => ReplT r s m a -> r -> s -> m (a, s)
runReplT n r s = do
  ref <- liftIO (newIORef s)
  res <- refReplT n r ref
  final <- liftIO (readIORef ref)
  pure (res, final)

-- | Directive to control voluntary REPL termination.
data ReplDirective
  = ReplQuit
  | ReplContinue
  deriving (Eq, Show)

-- | Run a simple REPL.
replM
  :: MonadUnliftIO m
  => ByteString                      -- ^ Prompt
  -> (ByteString -> m ReplDirective) -- ^ Action
  -> (ByteString -> m [ByteString])  -- ^ Completion
  -> m ()
replM prompt action comp = loop where
  loop = do
    Unlift.setCompletion comp
    res <- Unlift.getInputLine prompt
    case res of
      Nothing -> pure ()
      Just line -> do
        directive <- action line
        Unlift.addHistory line
        case directive of
          ReplContinue -> loop
          ReplQuit -> pure ()
