{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
module JS.DOM.EventTarget
    ( -- | constructor is not exported
      EventTarget
    , IEventTarget(..)
    )
    where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.Event
import JS.DOM.EventTarget.Internal

-- | The object that dispatched the event.
-- https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
class IObject j => IEventTarget j where
    addEventListener :: MonadIO m => j -> JSString -> Callback (JSVal -> IO ()) -> m ()
    addEventListener j n cb = liftIO $ js_addEventListener (toJS j) n cb

    removeEventListener :: MonadIO m => j -> JSString -> Callback (JSVal -> IO ()) -> m ()
    removeEventListener j n cb = liftIO $ js_removeEventListener (toJS j) n cb

    -- | throws, therefore @foreign import javascript safe@ so exceptions can be
    -- handled without killing haskell thread
    dispatchEvent :: MonadIO m => j -> Event -> m ()
    dispatchEvent j e = liftIO $ js_dispatchEvent (toJS j) e

instance IEventTarget EventTarget

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.addEventListener($2, $3);"
    js_addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
    "$1.removeEventListener($2, $3);"
    js_removeEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe -- since 'dispatchEvent throws
    "$1.dispatchEvent($2);"
    js_dispatchEvent :: JSVal -> Event -> IO ()

#else

js_addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()
js_addEventListener _ _ _ = pure ()

js_removeEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()
js_removeEventListener _ _ _ = pure ()

js_dispatchEvent :: JSVal -> Event -> IO ()
js_dispatchEvent _ _ = pure ()

#endif
