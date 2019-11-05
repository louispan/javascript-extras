{-# LANGUAGE CPP #-}

module JS.DOM.EventTarget.Node.Document
    ( -- | constructor not exported
      Document
    , IDocument(..)
    -- , globalDocument
    ) where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.EventTarget.Node
import JS.DOM.EventTarget.Node.Document.Internal
import JS.DOM.EventTarget.Node.Element
import JS.DOM.EventTarget.Window.Internal

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document
class INode j => IDocument j where
    defaultView :: MonadIO m => j -> m (Maybe Window)
    defaultView = liftIO . fmap fromJS . js_defaultView . toJS

    getElementById :: MonadIO m => j -> JSString -> m (Maybe Element)
    getElementById j i = liftIO $ fromJS <$> js_getElementById (toJS j) i

instance IDocument Document

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1.defaultView"
    js_defaultView :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$r = $1.getElementById($2)"
    js_getElementById :: JSVal -> JSString -> IO JSVal

#else

js_defaultView :: JSVal -> IO JSVal
js_defaultView _ = pure nullRef

js_getElementById :: JSVal -> JSString -> IO JSVal
js_getElementById _ _ = pure nullRef

#endif
