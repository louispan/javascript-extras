{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module JS.Data.Instances where

import Data.Hashable
import qualified Data.JSString as J
import Data.String
import GHCJS.Types
import JS.Cast

instance Hashable JSString where
    hashWithSalt s x = hashWithSalt s (J.unpack x)
    hash = hash . J.unpack

instance Show JSVal where
    show = maybe "(unknown)" J.unpack . fromJS . js_stringify

instance IsString JSVal where
    fromString = jsval . J.pack

#ifdef __GHCJS__

-- | may return null if stringify fails
foreign import javascript unsafe
  "$r = hje$stringify($1);"
  js_stringify :: JSVal -> JSVal


#else

js_stringify :: JSVal -> JSVal
js_stringify = id

#endif
