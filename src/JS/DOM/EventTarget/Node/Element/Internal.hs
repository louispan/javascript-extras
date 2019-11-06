{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Node.Element.Internal
    ( Element(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.EventTarget
import JS.DOM.EventTarget.Node

newtype Element = Element JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS Element where
    fromJS a | js_isElement a = Just $ Element a
    fromJS _ = Nothing

instance IObject Element
instance IEventTarget Element
instance INode Element

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof Element;"
    js_isElement :: JSVal -> Bool

#else

js_isElement :: JSVal -> Bool
js_isElement _ = False

#endif
