{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.Data.Object.Internal
    ( JSObject(..)
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.String
import qualified GHC.Generics as G
import GHCJS.Types
import JS.Data.Cast
import JS.Data.Instances

newtype JSObject = JSObject JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS JSObject where
    validFromJS = js_isObject
    fromJS a | js_isObject a = Just $ JSObject a
    fromJS _ = Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = typeof $1 === 'object';"
    js_isObject :: JSVal -> Bool

#else

js_isObject :: JSVal -> Bool
js_isObject _ = False

#endif
