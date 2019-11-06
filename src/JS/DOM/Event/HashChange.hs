{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.DOM.Event.HashChange
  ( -- | constructor not exported
    HashChangeEvent
  , IHashChangeEvent(..)
  )
where

import JS.Data
import JS.DOM.Event
import JS.DOM.Event.HashChange.Internal

-- | https://developer.mozilla.org/en-US/docs/Web/API/HashChangeEvent
-- https://reactjs.org/docs/events.html#HashChange-events
-- onScroll
class IEvent j => IHashChangeEvent j where
  oldURL :: j -> JSString
  oldURL = js_oldURL . toJS

  newURL :: j -> JSString
  newURL  = js_newURL . toJS

instance IHashChangeEvent HashChangeEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1.oldURL;"
    js_oldURL :: JSVal -> JSString

foreign import javascript unsafe
    "$r = $1.newURL;"
    js_newURL :: JSVal -> JSString

#else

js_oldURL :: JSVal -> JSString
js_oldURL _ = mempty

js_newURL :: JSVal -> JSString
js_newURL _ = mempty

#endif
