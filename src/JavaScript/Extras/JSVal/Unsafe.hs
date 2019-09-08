{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module JavaScript.Extras.JSVal.Unsafe where

import Data.JSString as J
import qualified GHCJS.Types as J

-- | Injection attack! Use with care
instance Read J.JSVal where
    readsPrec _ str = [(js_eval (J.pack str), [])]

#ifdef __GHCJS__

-- | Injection attack! Use with care
foreign import javascript unsafe
  "$r = hje$unstringify($1);"
  js_eval :: J.JSString -> J.JSVal

#else

js_eval :: J.JSString -> J.JSVal
js_eval _ = J.nullRef

#endif
