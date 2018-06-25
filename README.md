[![Hackage](https://img.shields.io/hackage/v/javascript-extras.svg)](https://hackage.haskell.org/package/javascript-extras)

Extra javascript functions when using GHCJS

# Changelog

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
