# linenoise

[![CircleCI](https://circleci.com/gh/ejconlon/haskell-linenoise/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/haskell-linenoise/tree/master)

A lightweight readline-replacement library for Haskell based on the `linenoise` library. (Not production ready!)
See the [demo app](https://github.com/ejconlon/haskell-linenoise/blob/master/app/Main.hs) for usage, or see the [climb](https://github.com/ejconlon/climb) library for higher-level building blocks for GHCi-like REPLs with colon-commands.

## Differences from alternatives

* `haskeline`
  * This uses FFI to a minimal C library vs pure Haskell
  * This uses standard MTL/Unlift typeclasses vs custom ones.
  * This does not require the use of a specific monad transformer.
* `readline`
  * This offers a `MonadIO/MonadUnliftIO` interface vs raw `IO`.
  * This vendors and statically links its underlying library to simplify the build process.

## License and attribution

This library includes the source code and license for `linenoise` in `cbits`.
It is a fork of an older [library](https://github.com/sdiehl/haskell-linenoise) with all licensing and
attribution preserved.

## Development workflow

This project uses `stack` to build.

The `Makefile` has a bunch of relevant phony targets for a development workflow including

* `build` - Build the library and demo
* `download` - Update vendored `linenoise`
* `demo` - Run the demo app
* `deps` - Install dev tools
* `lint` - Lint with `hlint`
* `format` - Format with `stylish-haskell`
* `docs` - Generate docs

In addition, there are targets starting with `ci` that are invoked in various CI phases.

## TODO

* Support unicode (may involve vendoring a `linenoise` [fork](https://github.com/yhirose/linenoise/tree/utf8-support))
* Verify that the FFI modifications for `ByteString` are memory-safe
