{-# LANGUAGE CPP #-}

module JavaScript.Extras.JSVar where

import Control.DeepSeq
import Data.Coerce
import Data.JSString as JS
import Data.String
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import JavaScript.Extras.Cast as JE

-- | Wrapper to have a JSVal that also have an IString instance
-- This is helpful when using OverloadedStrings
newtype JSVar = JSVar J.JSVal

instance Show JSVar where
    show = JS.unpack . js_stringify

instance J.IsJSVal JSVar

instance J.PToJSVal JSVar where
    pToJSVal = J.jsval

instance JE.ToJS JSVar

instance JE.FromJS JSVar where
    fromJS v = coerce (fromJS v :: Maybe J.JSVal)

instance IsString JSVar where
    fromString = JSVar . J.jsval . JS.pack

instance NFData JSVar where
    rnf (JSVar v) = rnf v

toJS' :: JE.ToJS a => a -> JSVar
toJS' = JSVar . toJS

fromJS' :: JE.FromJS a => JSVar -> Maybe a
fromJS' (JSVar v) = fromJS v

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hje$stringify($1);"
  js_stringify :: JSVar -> J.JSString

#else

js_stringify :: JSVar -> J.JSString
js_stringify _ = JS.empty

#endif
