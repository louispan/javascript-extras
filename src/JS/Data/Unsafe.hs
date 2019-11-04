{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module JS.Data.Unsafe where

import Data.JSString as J
import GHCJS.Types

-- | Injection attack! Use with care
instance Read JSVal where
    readsPrec _ str = [(js_eval (J.pack str), [])]

#ifdef __GHCJS__

-- | Injection attack! Use with care
foreign import javascript unsafe
  "$r = hje$unstringify($1);"
  js_eval :: JSString -> JSVal

#else

js_eval :: JSString -> JSVal
js_eval _ = nullRef

#endif
