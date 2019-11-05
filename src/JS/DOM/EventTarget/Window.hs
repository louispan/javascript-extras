{-# LANGUAGE CPP #-}

module JS.DOM.EventTarget.Window
    ( -- | constructor not exported
      Window
    , IWindow(..)
    -- , globalWindow
    ) where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.EventTarget
import JS.DOM.EventTarget.Node.Document.Internal
import JS.DOM.EventTarget.Window.Internal

-- https://developer.mozilla.org/en-US/docs/Web/API/Window
class IEventTarget j => IWindow j where
    document :: MonadIO m => j -> m (Maybe Document)
    document = liftIO . fmap fromJS . js_document . toJS

instance IWindow Window

#ifdef __GHCJS__


foreign import javascript unsafe
    "$1.document"
    js_document :: JSVal -> IO JSVal

#else

js_document :: JSVal -> IO JSVal
js_document _ = pure nullRef

#endif
