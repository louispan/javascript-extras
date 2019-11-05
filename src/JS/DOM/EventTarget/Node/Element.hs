{-# LANGUAGE CPP #-}

module JS.DOM.EventTarget.Node.Element
    ( -- | constructor not exported
      Element
    , IElement(..)
    ) where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.EventTarget.Node
import JS.DOM.EventTarget.Node.Element.Internal
import Prelude hiding (id)

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element
class INode j => IElement j where
    className :: MonadIO m => j -> m JSString
    className = liftIO . js_className . toJS

    id :: MonadIO m => j -> m JSString
    id = liftIO . js_id . toJS

instance IElement Element

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.className"
    js_className :: JSVal -> IO JSString

foreign import javascript unsafe
    "$1.id"
    js_id :: JSVal -> IO JSString

#else

js_className :: JSVal -> IO JSString
js_className _ = pure mempty

js_id :: JSVal -> IO JSString
js_id _ = pure mempty

#endif
