{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module JavaScript.Extras.JSVal.Instances where

import Data.String
import qualified Data.JSString as J
import qualified GHCJS.Types as J
import JavaScript.Extras.Cast as JE

instance Show J.JSVal where
    show = maybe "(undefined)" J.unpack . fromJS . js_stringify

instance IsString J.JSVal where
    fromString = J.jsval . J.pack

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hje$stringify($1);"
  js_stringify :: J.JSVal -> J.JSVal

#else

js_stringify :: J.JSVal -> J.JSVal
js_stringify = id

#endif
