{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.DOM.Event.UI
  ( -- | constructor not exported
    UIEvent
  , IUIEvent(..)
  )
where

import JS.Data
import JS.DOM.Event
import JS.DOM.Event.UI.Internal
import JS.DOM.EventTarget.Window.Internal

-- | https://developer.mozilla.org/en-US/docs/Web/API/UIEvent
-- https://reactjs.org/docs/events.html#ui-events
-- onScroll
class IEvent j => IUIEvent j where
  detail :: j -> Int
  detail = js_detail . toJS

  view :: j -> Maybe Window
  view  = fromJS . js_view . toJS

instance IUIEvent UIEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1.detail;"
    js_detail :: JSVal -> Int

foreign import javascript unsafe
    "$r = $1.view;"
    js_view :: JSVal -> JSVal
#else

js_detail :: JSVal -> Int
js_detail _ = 0

js_view :: JSVal -> JSVal
js_view _ = nullRef

#endif
