{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module JavaScript.Extras.JSVar.Unsafe where

import Data.JSString as JS
import qualified GHCJS.Types as J
import JavaScript.Extras.JSVar as JE

-- | Injection attack! Use with care
instance Read JE.JSVar where
    readsPrec _ str = [(js_eval (JS.pack str), [])]

#ifdef __GHCJS__

-- | Injection attack! Use with care
foreign import javascript unsafe
  "$r = hje$unstringify($1);"
  js_eval :: J.JSString -> JSVar

#else

js_eval :: J.JSString -> JSVar
js_eval _ = JSVar J.nullRef

#endif
