{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.DOM.Event.UI.Keyboard
  ( -- | constructor not exported
    KeyboardEvent
  , IKeyboardEvent(..)
  , ICommonKeyboardEvent(..)
  )
where

import JS.Data
import JS.DOM.Event.UI
import JS.DOM.Event.UI.Keyboard.Internal

-- | common api between 'IKeyboardEvent' and 'JS.DOM.Event.UI.Mouse.IMouseEvent'
class IUIEvent j => ICommonKeyboardEvent j where
  altKey :: j -> Bool
  altKey = js_altKey . toJS

  ctrlKey :: j -> Bool
  ctrlKey = js_ctrlKey . toJS

  getModifierState :: j -> JSString -> Bool
  getModifierState j n = js_getModifierState (toJS j) n

  metaKey :: j -> Bool
  metaKey = js_metaKey . toJS

  shiftKey :: j -> Bool
  shiftKey = js_shiftKey . toJS

instance ICommonKeyboardEvent KeyboardEvent

-- | Keyboard events
-- https://facebook.github.io/react/docs/events.html#keyboard-events
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
-- Event names (eventType)
-- onKeyDown (keydown) onKeyPress (keypress) onKeyUp (keyyp)
class ICommonKeyboardEvent j => IKeyboardEvent j where
  charCode :: j -> Int
  charCode = js_charCode . toJS

  key :: j -> JSString
  key = js_key . toJS

  keyCode :: j -> Int
  keyCode = js_keyCode . toJS

  locale :: j -> JSString
  locale = js_locale . toJS

  location :: j -> Int
  location = js_location . toJS

  repeat :: j -> Bool
  repeat = js_repeat . toJS

  which :: j -> Int
  which = js_which . toJS

instance IKeyboardEvent KeyboardEvent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = $1.altKey;"
    js_altKey :: JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.charCode;"
    js_charCode :: JSVal -> Int

foreign import javascript unsafe
    "$r = $1.ctrlKey;"
    js_ctrlKey :: JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.getModifierState($2);"
    js_getModifierState :: JSVal -> JSString -> Bool

foreign import javascript unsafe
    "$r = $1.key;"
    js_key :: JSVal -> JSString

foreign import javascript unsafe
    "$r = $1.keyCode;"
    js_keyCode :: JSVal -> Int

foreign import javascript unsafe
    "$r = $1.locale;"
    js_locale :: JSVal -> JSString

foreign import javascript unsafe
    "$r = $1.location;"
    js_location :: JSVal -> Int

foreign import javascript unsafe
    "$r = $1.metaKey;"
    js_metaKey :: JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.repeat;"
    js_repeat :: JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.shiftKey;"
    js_shiftKey :: JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.which;"
    js_which :: JSVal -> Int

#else

js_altKey :: j -> Bool
js_altKey _ = False

js_charCode :: j -> Int
js_charCode _ = 0

js_ctrlKey :: j -> Bool
js_ctrlKey _ = False

js_getModifierState :: j -> JSString -> Bool
js_getModifierState _ _ = False

js_key :: j -> JSString
js_key _ = mempty

js_keyCode :: j -> Int
js_keyCode _ = 0

js_locale :: j -> JSString
js_locale _ = mempty

js_location :: j -> Int
js_location _ = 0

js_metaKey :: j -> Bool
js_metaKey _ = False

js_repeat :: j -> Bool
js_repeat _ = False

js_shiftKey :: j -> Bool
js_shiftKey _ = False

js_which :: j -> Int
js_which _ = 0

#endif
