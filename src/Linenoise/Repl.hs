{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Helper functions and transformer to write your own REPLs.
module Linenoise.Repl
  ( ReplDirective (..)
  , ReplT (..)
  , replM
  , runReplT
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Zip (MonadZip)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Linenoise.Unlift (InputResult (..))
import qualified Linenoise.Unlift as Unlift

-- | Basic monad transformer with mutable state that can be used with all "Linenoise.Unlift" functions.
--   You do not have to use this, but it's here to cover most of what you would need without having
--   to roll your own newtype.
newtype ReplT r s m a = ReplT {unReplT :: ReaderT r (ReaderT (IORef s) m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadFix
    , MonadZip
    , MonadFail
    , MonadThrow
    , MonadCatch
    , MonadReader r
    )

askRef :: (Applicative m) => ReplT r s m (IORef s)
askRef = ReplT (ReaderT (const (ReaderT pure)))

refReplT :: ReplT r s m a -> r -> IORef s -> m a
refReplT n r = runReaderT (runReaderT (unReplT n) r)

instance MonadTrans (ReplT r s) where
  lift = ReplT . lift . lift

instance (MonadUnliftIO m) => MonadUnliftIO (ReplT r s m) where
  withRunInIO run = do
    r <- ask
    ref <- askRef
    wrappedWithRunInIO lift (\n -> refReplT n r ref) run

instance (MonadIO m) => MonadState s (ReplT r s m) where
  get = ReplT (ReaderT (const (ReaderT (liftIO . readIORef))))
  put s = ReplT (ReaderT (const (ReaderT (\ref -> liftIO (writeIORef ref s)))))

-- | Run a ReplT.
runReplT :: (MonadIO m) => ReplT r s m a -> r -> s -> m (a, s)
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
  :: (MonadUnliftIO m)
  => ReplDirective
  -- ^ Directive on interrupt
  -> Text
  -- ^ Prompt
  -> (Text -> m ReplDirective)
  -- ^ Action
  -> (Text -> m [Text])
  -- ^ Completion
  -> m ()
replM onInterrupt prompt action comp = loop
 where
  loop = do
    Unlift.setCompletion comp
    res <- Unlift.getInputLine prompt
    directive <- case res of
      InterruptResult -> pure onInterrupt
      EofResult -> pure ReplQuit
      LineResult line -> do
        directive <- action line
        Unlift.addHistory line
        pure directive
    case directive of
      ReplContinue -> loop
      ReplQuit -> pure ()
