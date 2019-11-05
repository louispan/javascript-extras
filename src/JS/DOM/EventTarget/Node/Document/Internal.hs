{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.EventTarget.Node.Document.Internal
    ( Document(..)
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data
import JS.DOM.EventTarget

newtype Document = Document JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS Document where
    fromJS a | js_isDocument a = Just $ Document a
    fromJS _ = Nothing

instance IObject Document
instance IEventTarget Document

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Document"
    js_isDocument :: JSVal -> Bool

#else

js_isDocument :: JSVal -> Bool
js_isDocument _ = False

#endif
