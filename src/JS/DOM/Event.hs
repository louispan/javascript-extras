{-# LANGUAGE CPP #-}

{-# LANGUAGE TypeApplications #-}

module JS.DOM.Event
    ( -- | constructor is not exported
      Event
    , IEvent(..)
    )
    where

import Control.Monad.IO.Class
import JS.Data
import JS.DOM.Event.Internal
import JS.DOM.EventTarget.Internal

-- Subset of https://developer.mozilla.org/en-US/docs/Web/API/Event
-- Not providing @isPropagationStopped@ as it is not supported natively by JavaScript
class IObject j => IEvent j where
    bubbles :: j -> Bool
    bubbles = js_bubbles . toJS

    cancelable :: j -> Bool
    cancelable =  js_cancelable . toJS

    currentTarget :: MonadIO m => j -> m (Maybe EventTarget)
    currentTarget = liftIO . fmap (fromJS @EventTarget) . js_currentTarget . toJS

    defaultPrevented  :: MonadIO m => j -> m Bool
    defaultPrevented = liftIO . js_defaultPrevented . toJS

    eventPhase :: MonadIO m => j -> m Int
    eventPhase = liftIO . js_eventPhase . toJS

    isTrusted :: j -> Bool
    isTrusted = js_isTrusted . toJS

    target :: MonadIO m => j -> m (Maybe EventTarget)
    target = liftIO . fmap (fromJS @EventTarget) . js_target . toJS

    timeStamp :: j -> Int
    timeStamp = js_timeStamp . toJS

    -- @type@ is a reserved word, so add a prefix to make 'eventType'
    eventType :: j -> JSString
    eventType = js_eventType . toJS

    preventDefault :: MonadIO m => j -> m ()
    preventDefault = liftIO . js_preventDefault . toJS

    stopPropagation :: MonadIO m => j -> m ()
    stopPropagation = liftIO . js_stopPropagation . toJS

instance IEvent Event

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.bubbles"
    js_bubbles :: JSVal -> Bool

foreign import javascript unsafe
    "$1.cancelable"
    js_cancelable :: JSVal -> Bool

foreign import javascript unsafe
    "$1.currentTarget"
    js_currentTarget :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1.preventDefault();"
    js_preventDefault :: JSVal -> IO ()

foreign import javascript unsafe
    "$1.eventPhase"
    js_eventPhase :: JSVal -> IO Int

foreign import javascript unsafe
    "$1.isTrusted"
    js_isTrusted :: JSVal -> Bool

foreign import javascript unsafe
    "$1.target"
    js_target :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1.timeStamp"
    js_timeStamp :: JSVal -> Int

foreign import javascript unsafe
    "$1.type"
    js_eventType :: JSVal -> JSString

foreign import javascript unsafe
    "$1.defaultPrevented"
    js_defaultPrevented :: JSVal -> IO Bool

foreign import javascript unsafe
    "$1.stopPropagation();"
    js_stopPropagation :: JSVal -> IO ()

#else

js_bubbles:: JSVal -> Bool
js_bubbles _ = False

js_cancelable:: JSVal -> Bool
js_cancelable _ = False

js_currentTarget :: JSVal -> IO JSVal
js_currentTarget _ = pure nullRef

js_defaultPrevented :: JSVal -> IO Bool
js_defaultPrevented _ = pure False

js_eventPhase :: JSVal -> IO Int
js_eventPhase _ = pure 0

js_isTrusted:: JSVal -> Bool
js_isTrusted _ = False

js_target :: JSVal -> IO JSVal
js_target _ = pure nullRef

js_timeStamp :: JSVal -> Int
js_timeStamp _ = 0

js_eventType :: JSVal -> JSString
js_eventType _ = mempty

js_preventDefault :: JSVal -> IO ()
js_preventDefault _ = pure ()

js_stopPropagation :: JSVal -> IO ()
js_stopPropagation _ = pure ()

#endif
