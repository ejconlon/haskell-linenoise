Repl
----

TODO
* rename
* proper license
* split unlift module
* support save
* create repl module with settings

Initial work on a lightweight readline library for Haskell based on the ``linenoise`` library. Designed from
the ground up to work more smoothly with modern monad transformers and exceptions libraries.

```haskell
import System.Console.Repl

type Repl = ReplT IO

completer :: String -> [String]
completer ('h':_) = ["hello", "hello there"]
completer _ = []

repl :: Repl ()
repl = replM ">>> " outputStrLn completer

main :: IO ()
main = runRepl repl defaultSettings
```

Can compose with the regular State monad, for instance to do stateful tab completion. Something which is
painful with Haskeline.

```haskell
import System.Console.Repl
import Control.Monad.State.Strict
import Data.List (isPrefixOf)

type Repl = ReplT (StateT [String] IO)

completer :: String -> Repl [String]
completer line = do
  comps <- get
  return $ filter (isPrefixOf line) comps

action :: String -> Repl ()
action x = do
  modify $ (x:)
  liftIO $ putStrLn x

repl :: Repl ()
repl = replM ">>> " action (byWord completer)

main :: IO ()
main = evalStateT (runRepl repl defaultSettings) []
```

License
-------

Includes the source code and license for linenoise in `cbits`. Released under the BSD license.

Copyright (c) 2014-2017, Stephen Diehl
