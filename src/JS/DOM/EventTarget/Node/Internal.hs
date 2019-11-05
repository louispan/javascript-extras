{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Node.Internal
    ( Node(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.EventTarget

newtype Node = Node JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS Node where
    fromJS a | js_isNode a = Just $ Node a
    fromJS _ = Nothing

instance IObject Node
instance IEventTarget Node

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Node"
    js_isNode :: JSVal -> Bool

#else

js_isNode :: JSVal -> Bool
js_isNode _ = False

#endif
