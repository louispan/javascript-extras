{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Node.Element.HTML.Internal
    ( HTMLElement(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.EventTarget
import JS.DOM.EventTarget.Node
import JS.DOM.EventTarget.Node.Element

newtype HTMLElement = HTMLElement JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS HTMLElement where
    fromJS a | js_isHTMLElement a = Just $ HTMLElement a
    fromJS _ = Nothing

instance IObject HTMLElement
instance IEventTarget HTMLElement
instance INode HTMLElement
instance IElement HTMLElement

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 !== 'undefined' && $1 instanceof HTMLElement;"
    js_isHTMLElement :: JSVal -> Bool

#else

js_isHTMLElement :: JSVal -> Bool
js_isHTMLElement _ = False

#endif
