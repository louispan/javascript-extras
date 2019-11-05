-- | You probably want to import this qualified as the DOM has lot of names that might clash
module JS.DOM
    (
      module JS.DOM.Event
    , module JS.DOM.Event.UI
    , module JS.DOM.Event.UI.Keyboard
    , module JS.DOM.Event.UI.Mouse
    , module JS.DOM.Event.HashChange
    , module JS.DOM.EventTarget
    , module JS.DOM.EventTarget.Node
    , module JS.DOM.EventTarget.Node.Document
    , module JS.DOM.EventTarget.Node.Element
    , module JS.DOM.EventTarget.Node.Element.HTML
    , module JS.DOM.EventTarget.Window
    ) where

import JS.DOM.Event
import JS.DOM.Event.HashChange
import JS.DOM.Event.UI
import JS.DOM.Event.UI.Keyboard
import JS.DOM.Event.UI.Mouse
import JS.DOM.EventTarget
import JS.DOM.EventTarget.Node
import JS.DOM.EventTarget.Node.Document
import JS.DOM.EventTarget.Node.Element
import JS.DOM.EventTarget.Node.Element.HTML
import JS.DOM.EventTarget.Window
