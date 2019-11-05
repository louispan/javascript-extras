{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TupleSections #-}

module JS.Data.Object
    ( JSObject
    , mkObject
    , globalThis
    , object_entries
    , object_fromEntries
    , IObject(..)
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import GHCJS.Types
import qualified JavaScript.Array.Internal as JAI
import JS.Data.Cast
import JS.Data.Object.Internal
import Unsafe.Coerce

-- | https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis
globalThis :: JSObject
globalThis = JSObject js_globalThis

mkObject :: MonadIO m => m JSObject
mkObject = liftIO js_mkObject

-- | https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object
object_entries :: (MonadIO m, IObject j) => j -> m [(JSVal, JSVal)]
object_entries o = liftIO $ (fmap f . JAI.toList . JAI.SomeJSArray) <$> js_object_entries (toJS o)
  where
    g :: [JSVal] -> (JSVal, JSVal)
    g [k, v] = (k, v)
    g _ = error "Object entries didn't return pair"
    f :: JSVal -> (JSVal, JSVal)
    f j = g $ JAI.toList $ JAI.SomeJSArray j

object_fromEntries :: (MonadIO m) => [(JSVal, JSVal)] -> m JSObject
object_fromEntries xs = liftIO $ js_object_fromEntries xs'
  where
    xs' = jsval $ JAI.fromList $ (g . f) <$> xs
    f :: (JSVal, JSVal) -> [JSVal]
    f (k, v) = [k, v]
    g :: [JSVal] -> JSVal
    g ys = jsval (JAI.fromList ys)

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
  "globalThis"
  js_globalThis :: JSVal

foreign import javascript unsafe
  "$r = {};"
  js_mkObject :: IO JSObject

foreign import javascript unsafe
  "Object.entries($1)"
  js_object_entries :: JSVal -> IO JSVal

foreign import javascript unsafe
  "Object.fromEntries($1)"
  js_object_fromEntries :: JSVal -> IO JSObject

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

#else

js_globalThis :: JSVal
js_globalThis = nullRef

js_mkObject :: IO JSObject
js_mkObject = pure $ JSObject nullRef

js_object_fromEntries :: JSVal -> IO JSObject
js_object_fromEntries _ = pure $ JSObject nullRef

-- | throws an exception if undefined or null
js_object_entries :: JSVal -> IO JSVal
js_object_entries _ = pure nullRef

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
