{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JS.Data.Object.Internal where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.String
import qualified GHC.Generics as G
import GHCJS.Types
import JS.Cast
import JS.Data.Instances

newtype JSObject = JSObject JSVal
    deriving (G.Generic, Show, ToJS, IsString, NFData)

instance FromJS JSObject where
    validFromJS = js_isObject
    fromJS a | js_isObject a = Just $ JSObject a
    fromJS _ = Nothing

mkObject :: MonadIO m => m JSObject
mkObject = liftIO js_mkObject

#ifdef __GHCJS__

foreign import javascript unsafe
    "typeof $1 === 'object'"
    js_isObject :: JSVal -> Bool

foreign import javascript unsafe
  "$r = {};"
  js_mkObject :: IO Object

#else

js_isObject :: JSVal -> Bool
js_isObject _ = False

js_mkObject :: IO JSObject
js_mkObject = pure $ JSObject nullRef

#endif
