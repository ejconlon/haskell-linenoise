{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.Repl (
  getInputLine,
  addHistory,
  setCompletion,
  replM,
  byWord
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified System.Console.FFI as FFI

-- Run the prompt, yielding a string.
getInputLine :: MonadIO m => ByteString -> m (Maybe ByteString)
getInputLine = liftIO . FFI.getInputLine

-- | Add to current history.
addHistory :: MonadIO m => ByteString -> m ()
addHistory = liftIO . FFI.addHistory

-- | Set the current completion function
setCompletion :: MonadUnliftIO m => (ByteString -> m [ByteString]) -> m ()
setCompletion = undefined

-- | Run a simple REPL.
replM
  :: MonadUnliftIO m
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
