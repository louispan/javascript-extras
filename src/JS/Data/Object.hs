{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TupleSections #-}

module JS.Data.Object
    ( JSObject
    , mkObject
    , IObject(..)
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import GHCJS.Types
import qualified JavaScript.Object as JO
import qualified JavaScript.Object.Internal as JOI
import JS.Cast
import JS.Data.Object.Internal
import Unsafe.Coerce

-- Subset of https://developer.mozilla.org/en-US/docs/Web/API/Event
-- Not providing @isPropagationStopped@ as it is not supported natively by JavaScript
class ToJS j => IObject j where

    -- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
    getProperty :: (MonadIO m, ToJS s) => j -> s -> m JSVal
    getProperty j k =
        if js_isInvalid j' || js_isInvalid k'
        then pure nullRef
        else liftIO $ js_unsafeGetProperty j' k'
      where
        k' = toJS k
        j' = toJS j

    deleteProperty :: (MonadIO m, ToJS s) => j -> s -> m ()
    deleteProperty j k =
        if js_isInvalid j' || js_isInvalid k'
        then pure ()
        else liftIO $ js_unsafeDeleteProperty j' k'
      where
        k' = toJS k
        j' = toJS j

    setProperty :: (MonadIO m, ToJS s) => j -> (s, JSVal) -> m ()
    setProperty j (k, v) =
        if js_isInvalid j' || js_isInvalid k'
        then pure ()
        else liftIO $ js_unsafeSetProperty j' k' v
      where
        k' = toJS k
        j' = toJS j

instance IObject JSObject

#ifdef __GHCJS__

foreign import javascript unsafe
  "typeof $1 === 'undefined' || $1 === null"
  js_isInvalid :: JSVal -> Bool

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "delete $1[$2];"
  js_unsafeDeleteProperty :: JSVal -> JSVal -> IO ()

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$1[$2]"
  js_unsafeGetProperty :: JSVal -> JSVal -> IO JSVal

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$1[$2] = $3;"
  js_unsafeSetProperty :: JSVal -> JSVal -> JSVal -> IO ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
foreign import javascript unsafe
  "$r = hje$fromHsZipListJSVal($1, $2);"
  js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object

#else

-- | throws an exception if undefined or null
js_isInvalid :: JSVal -> Bool
js_isInvalid _ = False

-- | throws an exception if undefined or null
js_unsafeDeleteProperty :: JSVal -> JSVal -> IO ()
js_unsafeDeleteProperty _ _ = pure ()

-- | throws an exception if undefined or null
js_unsafeGetProperty :: JSVal -> JSVal -> IO JSVal
js_unsafeGetProperty _ _ = pure $ nullRef

-- | throws an exception if undefined or null
js_unsafeSetProperty :: JSVal -> JSVal -> JSVal -> IO ()
js_unsafeSetProperty _ _ _ = pure ()

#endif
