{-# LANGUAGE CPP #-}

module JavaScript.Extras.Number
    ( maxSafeInteger
    , minSafeInteger
    , safeModularIncrement
    , safeModularDecrement
    ) where

maxSafeInteger :: Int
maxSafeInteger = js_maxSafeInteger

minSafeInteger :: Int
minSafeInteger = js_minSafeInteger

-- | always returns a nubber between [Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER]
safeModularIncrement :: Int -> Int
safeModularIncrement i = if i >= maxSafeInteger then minSafeInteger else i + 1

-- | always returns a number between [Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER]
safeModularDecrement :: Int -> Int
safeModularDecrement i = if i <= minSafeInteger then maxSafeInteger else i - 1

#ifdef __GHCJS__

foreign import javascript unsafe
  "Number.MAX_SAFE_INTEGER"
  js_maxSafeInteger :: Int

foreign import javascript unsafe
  "Number.MIN_SAFE_INTEGER"
  js_maxSafeInteger :: Int

#else

js_maxSafeInteger :: Int
js_maxSafeInteger = 0

js_minSafeInteger :: Int
js_minSafeInteger = 0

#endif
