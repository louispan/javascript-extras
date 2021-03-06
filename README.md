[![Hackage](https://img.shields.io/hackage/v/javascript-extras.svg)](https://hackage.haskell.org/package/javascript-extras)

Extra javascript functions when using GHCJS

Please have a look at [example README](https://github.com/louispan/javascript-extras/blob/master/example/README.md) for an example of how to bundle this in your web app.

# Changelog

* 0.6.0.0
  - renamed `getProperty` and `setProperty` to `getPropertyIO` and `setPropertyIO`, and made the JVal to modify the first argument (better for mutating method)
  - Added instances for `Data.Aeson` and `Data.Aeson.Applicative`.
  - Removed JSRep; using orphan instances of JSVal instead.
  - Using ghcjs-base-stub-0.3.0.0

* 0.5.0.0
  - flipped the args of `getProperty` and `setProperty`
    This makes it easier for chaining.

* 0.4.0.0
  - Renamed `JSVar` to `JSRep` to avoid confusion with `JSVal`
  - Renamed `toJS'` to `toJSR`
  - Renamed `fromJS'` to `fromJSR`
  - `getProperty` and `setProperty` uses `JE.ToJS a` instead of `Coercible a J.JSVal`
  - flipped the args of `getProperty` and `setProperty`
  - Moved `justSnds` to esoteric-extras `Data.Maybe.Esoteric.keepMaybes`
  - Renamed `safeModularIncrement` to `safeIncrement`
  - Renamed `safeModularDecrement` to `safeDecrement`

* 0.3.3.0
  - Added `classNames`, `justSnds`
