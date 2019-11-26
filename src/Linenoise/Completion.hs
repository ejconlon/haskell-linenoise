{-# LANGUAGE OverloadedStrings #-}

module Linenoise.Completion
  ( byWord
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

-- | Complete by word
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
        [] -> pure [line]
        [y] ->
          pure [BSC.unwords xs <> " " <> x <> trimComplete x y <> " "]
        ys ->
          pure (map (complete x xs) ys)

complete :: ByteString -> [ByteString] -> ByteString -> ByteString
complete x xs y =
  BSC.unwords xs <> " " <> x <> trimComplete x y

trimComplete :: ByteString -> ByteString -> ByteString
trimComplete = BSC.drop . BSC.length
