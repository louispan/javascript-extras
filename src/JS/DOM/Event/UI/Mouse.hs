{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.DOM.Event.UI.Mouse
  ( -- | constructor not exported
    MouseEvent
  , IMouseEvent(..)
  )
where

import JS.Data
import JS.DOM.Event.UI.Keyboard
import JS.DOM.Event.UI.Mouse.Internal
import JS.DOM.EventTarget.Internal

-- | Mouse and Drag/Drop events
-- https://facebook.github.io/react/docs/events.html#mouse-events
-- https://developer.mozilla.org/en-US/docs/Web/Events
-- Event names (eventType)
-- onClick (click) onContextMenu (contextmenu) onDoubleClick (dblclick)
-- onDrag (drag) onDragEnd (dragend) onDragEnter (dragenter) onDragExit (dragexit)
-- onDragLeave (dragleave) onDragOver (dragover) onDragStart (dragstart)
-- onDrop (drop) onMouseDown (mousedown) onMouseEnter (mouseenter) onMouseLeave (mouseleave)
-- onMouseMove (mousemove) onMouseOut (mouseout) onMouseOver (mouseover) onMouseUp (mouseup)
class ICommonKeyboardEvent j => IMouseEvent j where
  button :: j -> Int
  button = js_button . toJS

  buttons :: j -> Int
  buttons = js_buttons . toJS

  clientX :: j -> Int
  clientX = js_clientX . toJS

  clientY :: j -> Int
  clientY = js_clientY . toJS

  pageX :: j -> Int
  pageX = js_pageX . toJS

  pageY :: j -> Int
  pageY = js_pageY . toJS

  relatedTarget :: j -> Maybe EventTarget
  relatedTarget = fromJS . js_relatedTarget . toJS

  screenX :: j -> Int
  screenX = js_screenX . toJS

  screenY :: j -> Int
  screenY = js_screenY . toJS

instance IMouseEvent MouseEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.button"
    js_button :: JSVal -> Int

foreign import javascript unsafe
    "$1.buttons"
    js_buttons :: JSVal -> Int

foreign import javascript unsafe
    "$1.clientX"
    js_clientX :: JSVal -> Int

foreign import javascript unsafe
    "$1.clientY"
    js_clientY :: JSVal -> Int

foreign import javascript unsafe
    "$1.pageX"
    js_pageX :: JSVal -> Int

foreign import javascript unsafe
    "$1.pageY"
    js_pageY :: JSVal -> Int

foreign import javascript unsafe
    "$1.relatedTarget"
    js_relatedTarget :: JSVal -> JSVal

foreign import javascript unsafe
    "$1.screenX"
    js_screenX :: JSVal -> Int

foreign import javascript unsafe
    "$1.screenY"
    js_screenY :: JSVal -> Int

#else

js_button :: JSVal -> Int
js_button _ = 0

js_buttons :: JSVal -> Int
js_buttons _ = 0

js_clientX :: JSVal -> Int
js_clientX _ = 0

js_clientY :: JSVal -> Int
js_clientY _ = 0

js_pageX :: JSVal -> Int
js_pageX _ = 0

js_pageY :: JSVal -> Int
js_pageY _ = 0

js_relatedTarget :: JSVal -> JSVal
js_relatedTarget _ = nullRef

js_screenX :: JSVal -> Int
js_screenX _ = 0

js_screenY :: JSVal -> Int
js_screenY _ = 0

#endif
