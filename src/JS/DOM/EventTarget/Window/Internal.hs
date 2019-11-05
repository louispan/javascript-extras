{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Window.Internal
    ( Window(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.EventTarget

newtype Window = Window JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS Window where
    fromJS a | js_isWindow a = Just $ Window a
    fromJS _ = Nothing

instance IObject Window
instance IEventTarget Window

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Window"
    js_isWindow :: JSVal -> Bool

#else

js_isWindow :: JSVal -> Bool
js_isWindow _ = False

#endif
