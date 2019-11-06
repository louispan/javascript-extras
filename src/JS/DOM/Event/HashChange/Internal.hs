{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.Event.HashChange.Internal
( HashChangeEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.Event

newtype HashChangeEvent = HashChangeEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS HashChangeEvent where
    validFromJS = js_isHashChangeEvent
    fromJS a | js_isHashChangeEvent a = Just $ HashChangeEvent a
    fromJS _ = Nothing

instance IObject HashChangeEvent
instance IEvent HashChangeEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof HashChangeEvent;"
    js_isHashChangeEvent :: JSVal -> Bool

#else

js_isHashChangeEvent :: JSVal -> Bool
js_isHashChangeEvent _ = False

#endif
