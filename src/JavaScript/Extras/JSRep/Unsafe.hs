{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module JavaScript.Extras.JSRep.Unsafe where

import Data.JSString as JS
import qualified GHCJS.Types as J
import JavaScript.Extras.JSRep as JE

-- | Injection attack! Use with care
instance Read JE.JSRep where
    readsPrec _ str = [(js_eval (JS.pack str), [])]

#ifdef __GHCJS__

-- | Injection attack! Use with care
foreign import javascript unsafe
  "$r = hje$unstringify($1);"
  js_eval :: J.JSString -> JSRep

#else

js_eval :: J.JSString -> JSRep
js_eval _ = JSRep J.nullRef

#endif
