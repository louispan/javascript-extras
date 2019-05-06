{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module JavaScript.Extras.JSRep where

import Control.Applicative
import Control.DeepSeq
import Control.Newtype.Generics
import Data.Coerce
import qualified Data.JSString as J
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
    show = maybe "undefined" J.unpack . fromJSRep . js_stringify

instance J.IsJSVal JSRep

instance J.PToJSVal JSRep where
    pToJSVal = J.jsval

instance JE.ToJS JSRep

instance JE.FromJS JSRep where
    fromJS v = coerce (fromJS v :: Maybe J.JSVal)

instance IsString JSRep where
    fromString = JSRep . J.jsval . J.pack

instance NFData JSRep where
    rnf (JSRep v) = rnf v

toJSRep :: JE.ToJS a => a -> JSRep
toJSRep = JSRep . toJS

whenJSRep :: (Alternative m, JE.ToJS a) => Maybe a -> m JSRep
whenJSRep = fmap toJSRep . maybe empty pure

-- | Remember 'Maybe' is also an instance of Alternative
fromJSRep :: (Alternative m, JE.FromJS a) => JSRep -> m a
fromJSRep (JSRep v) = maybe empty pure $ fromJS v

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = hje$stringify($1);"
  js_stringify :: JSRep -> JSRep

#else

js_stringify :: JSRep -> JSRep
js_stringify _ = JSRep J.nullRef

#endif
