{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module JavaScript.Extras.JSRep where

import Control.DeepSeq
import Control.Newtype.Generics
import Data.Coerce
import Data.JSString as JS
import Data.String
import GHC.Generics
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import JavaScript.Extras.Cast as JE

-- | Wrapper to have a JSVal that also have an IString instance
-- This is helpful when using OverloadedStrings
newtype JSRep = JSRep J.JSVal deriving (Generic)

instance Newtype JSRep

instance Show JSRep where
    show = JS.unpack . js_stringify

instance J.IsJSVal JSRep

instance J.PToJSVal JSRep where
    pToJSVal = J.jsval

instance JE.ToJS JSRep

instance JE.FromJS JSRep where
    fromJS v = coerce (fromJS v :: Maybe J.JSVal)

instance IsString JSRep where
    fromString = JSRep . J.jsval . JS.pack

instance NFData JSRep where
    rnf (JSRep v) = rnf v

toJSR :: JE.ToJS a => a -> JSRep
toJSR = JSRep . toJS

fromJSR :: JE.FromJS a => JSRep -> Maybe a
fromJSR (JSRep v) = fromJS v

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hje$stringify($1);"
  js_stringify :: JSRep -> J.JSString

#else

js_stringify :: JSRep -> J.JSString
js_stringify _ = JS.empty

#endif
