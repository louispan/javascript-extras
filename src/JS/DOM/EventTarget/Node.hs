{-# LANGUAGE CPP #-}

module JS.DOM.EventTarget.Node
    ( -- | constructor not exported
      Node
    , INode(..)
    ) where

import JS.Data
import JS.DOM.EventTarget
import JS.DOM.EventTarget.Node.Document.Internal
import JS.DOM.EventTarget.Node.Internal

-- | https://developer.mozilla.org/en-US/docs/Web/API/Node
class IEventTarget j => INode j where
    -- | Returns a DOMString containing the name of the Node. The structure of the name will differ with the node type. E.g. An HTMLElement will contain the name of the corresponding tag, like 'audio' for an HTMLAudioElement, a Text node will have the '#text' string, or a Document node will have the '#document' string.
    nodeName :: j -> JSString
    nodeName = js_nodeName . toJS

    -- | Returns an unsigned short representing the type of the node. Possible values are:
    -- Name    Value
    -- ELEMENT_NODE    1
    -- ATTRIBUTE_NODE  2 -- DEPRECATED
    -- TEXT_NODE   3
    -- CDATA_SECTION_NODE  4
    -- ENTITY_REFERENCE_NODE   5 -- DEPRECATED
    -- ENTITY_NODE     6 -- DEPRECATED
    -- PROCESSING_INSTRUCTION_NODE 7
    -- COMMENT_NODE    8
    -- DOCUMENT_NODE   9
    -- DOCUMENT_TYPE_NODE  10
    -- DOCUMENT_FRAGMENT_NODE  11
    -- NOTATION_NODE   12 -- DEPRECATED
    nodeType :: j -> Int
    nodeType = js_nodeType . toJS

    -- Returns the Document that this node belongs to. If the node is itself a document, returns null.
    ownerDocument :: j -> Maybe Document
    ownerDocument = fromJS . js_ownerDocument . toJS

instance INode Node
instance INode Document

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1.nodeName"
    js_nodeName :: JSVal -> JSString

foreign import javascript unsafe
    "$1.nodeType"
    js_nodeType :: JSVal -> Int

foreign import javascript unsafe
    "$1.ownerDocument"
    js_ownerDocument :: JSVal -> JSVal

#else

js_nodeName :: JSVal -> JSString
js_nodeName _ = mempty

js_nodeType :: JSVal -> Int
js_nodeType _ = 0

js_ownerDocument :: JSVal -> JSVal
js_ownerDocument _ = nullRef

#endif
