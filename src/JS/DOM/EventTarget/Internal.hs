{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Internal
    ( EventTarget(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data

newtype EventTarget = EventTarget JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS EventTarget where
    validFromJS = js_isEventTarget
    fromJS a | js_isEventTarget a = Just $ EventTarget a
    fromJS _ = Nothing

instance IObject EventTarget

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof EventTarget"
    js_isEventTarget :: JSVal -> Bool

#else

js_isEventTarget :: JSVal -> Bool
js_isEventTarget _ = False

#endif
