# Changelog

* 1.0.0.0
  - changed namespace from JavaScript to JS
  - changed goal to be an opinionated JS prelude instead of ghcjs-base
  - setProperty, etc are now part of an IObject class; also fix is_invalid bug

* 0.8.0.0
  - `setProperty` uses a tuple again

* 0.7.0.0
  - `setProperty` uses two arguments instead of a tuple
  - Changed semantics of 'FromJS' for 'JSVal' and 'ToJS/FromJS' for 'Maybe a' to make them symmetric

* 0.6.0.0
  - `getProperty` and `setProperty` argument order has the JVal to modify the first argument (better for mutations under the IO monad)
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
