{-# LANGUAGE CPP #-}

module JavaScript.Extras.Number
    ( maxSafeInteger
    ) where

maxSafeInteger :: Int
maxSafeInteger = js_maxSafeInteger

#ifdef __GHCJS__

foreign import javascript unsafe
  "Number.MAX_SAFE_INTEGER"
  js_maxSafeInteger :: Int64

#else

js_maxSafeInteger :: Int
js_maxSafeInteger = 0

#endif
