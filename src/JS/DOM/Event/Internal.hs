{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.DOM.Event.Internal
( Event(..)
) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import JS.Data

-- | The native event
newtype Event = Event JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS Event where
    validFromJS = js_isEvent
    fromJS a | js_isEvent a = Just $ Event a
    fromJS _ = Nothing

instance IObject Event

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 !== 'undefined' && $1 instanceof Event"
    js_isEvent :: JSVal -> Bool

#else

js_isEvent :: JSVal -> Bool
js_isEvent _ = False

#endif
