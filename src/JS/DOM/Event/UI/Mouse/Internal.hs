{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.Event.UI.Mouse.Internal
( MouseEvent(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.Event
import JS.DOM.Event.UI
import JS.DOM.Event.UI.Keyboard

newtype MouseEvent = MouseEvent JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS MouseEvent where
    validFromJS = js_isMouseEvent
    fromJS a | js_isMouseEvent a = Just $ MouseEvent a
    fromJS _ = Nothing

instance IObject MouseEvent
instance IEvent MouseEvent
instance IUIEvent MouseEvent
instance ICommonKeyboardEvent MouseEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof MouseEvent;"
    js_isMouseEvent :: JSVal -> Bool

#else

js_isMouseEvent :: JSVal -> Bool
js_isMouseEvent _ = False

#endif
