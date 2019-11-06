{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.Event.UI.Keyboard.Internal
( KeyboardEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.Event
import JS.DOM.Event.UI

newtype KeyboardEvent = KeyboardEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS KeyboardEvent where
    validFromJS = js_isKeyboardEvent
    fromJS a | js_isKeyboardEvent a = Just $ KeyboardEvent a
    fromJS _ = Nothing

instance IObject KeyboardEvent
instance IEvent KeyboardEvent
instance IUIEvent KeyboardEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof KeyboardEvent;"
    js_isKeyboardEvent :: JSVal -> Bool

#else

js_isKeyboardEvent :: JSVal -> Bool
js_isKeyboardEvent _ = False

#endif
