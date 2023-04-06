{-# LANGUAGE OverloadedStrings #-}

module Linenoise.Completion
  ( byWord
  )
where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Complete by word.
byWord :: Monad m => (Text -> m [Text]) -> (Text -> m [Text])
byWord f line = do
  let split = Text.words line
  case split of
    [] -> f line
    [_] -> f line
    sp -> do
      let (x, xs) = (last sp, init sp)
      res <- f x
      case res of
        [] -> pure [line]
        [y] ->
          pure [Text.unwords xs <> " " <> x <> trimComplete x y <> " "]
        ys ->
          pure (map (complete x xs) ys)

complete :: Text -> [Text] -> Text -> Text
complete x xs y =
  Text.unwords xs <> " " <> x <> trimComplete x y

trimComplete :: Text -> Text -> Text
trimComplete = Text.drop . Text.length
