# linenoise

A lightweight readline-replacement library for Haskell based on the `linenoise` library. (NOT PRODUCTION READY!)

See the [demo app](https://github.com/ejconlon/haskell-linenoise/blob/master/app/Main.hs).

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

The `Makefile` has a bunch of relevant phony targets for a development workflow including

* `download` - Update vendored `linenoise`
* `demo` - Run the demo app
* `deps` - Install dev tools
* `lint` - Lint with `hlint`
* `format` - Format with `stylish-haskell`
* `cisetup` - Invoked by CI to setup GHC
* `citest` - Invoked by CI to test (build and lint)

## TODO

* Support unicode (may involve vendoring a `linenoise` [fork](https://github.com/yhirose/linenoise/tree/utf8-support))
* Verify that my FFI modifications for `ByteString` are memory-safe
* Expose more of the `linenoise` API
* Upload to Hackage
