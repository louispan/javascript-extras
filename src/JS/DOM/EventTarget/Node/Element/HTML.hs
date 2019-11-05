{-# LANGUAGE CPP #-}

module JS.DOM.EventTarget.Node.Element.HTML
    ( -- | constructor not exported
      HTMLElement
    , IHTMLElement(..)
    ) where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.EventTarget.Node.Element
import JS.DOM.EventTarget.Node.Element.HTML.Internal
import Prelude hiding (id)

-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
class IElement j => IHTMLElement j where
    focus :: MonadIO m => j -> m ()
    focus = liftIO . js_focus . toJS

    blur :: MonadIO m => j -> m ()
    blur = liftIO . js_blur . toJS

instance IHTMLElement HTMLElement

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.focus();"
    js_focus :: JSVal -> IO ()

foreign import javascript unsafe
    "$1.blur();"
    js_blur :: JSVal -> IO ()

#else

js_focus :: JSVal -> IO ()
js_focus _ = pure ()

js_blur :: JSVal -> IO ()
js_blur _ = pure ()

#endif
