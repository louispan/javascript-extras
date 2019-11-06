{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.Event.UI.Internal
( UIEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.Event

newtype UIEvent = UIEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS UIEvent where
    validFromJS = js_isUIEvent
    fromJS a | js_isUIEvent a = Just $ UIEvent a
    fromJS _ = Nothing

instance IObject UIEvent
instance IEvent UIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof UIEvent;"
    js_isUIEvent :: JSVal -> Bool

#else

js_isUIEvent :: JSVal -> Bool
js_isUIEvent _ = False

#endif
