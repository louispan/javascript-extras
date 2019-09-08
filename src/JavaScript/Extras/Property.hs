{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TupleSections #-}

module JavaScript.Extras.Property
    ( getPropertyIO
    , setPropertyIO
    , propertiesToObject
    , objectToProperties
    ) where

import Control.DeepSeq
import Control.Parallel
import qualified GHC.Exts as Exts
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras.Cast as JE
import qualified JavaScript.Object as JO
import qualified JavaScript.Object.Internal as JOI
import Unsafe.Coerce

-- type Property = (J.JSString, JE.JSRep)

-- -- | Creates a JE.JSRep single string for "className" property from a list of (JSString, Bool)
-- -- Idea from https://github.com/JedWatson/classnames
-- classNames :: [(J.JSString, Bool)] -> JE.JSRep
-- classNames = JE.toJSRep . JS.unwords . fmap fst . filter snd

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getPropertyIO :: JE.ToJS j => J.JSString -> j -> IO J.JSVal
getPropertyIO k j =
    if J.isUndefined x || J.isNull x
        || J.isUndefined k' || J.isNull k'
    then pure $ J.nullRef
    else js_unsafeGetProperty k x
  where
    k' = J.pToJSVal k
    x = JE.toJS j
-- | set a property of any JSVal
setPropertyIO :: JE.ToJS j => (J.JSString, J.JSVal) -> j -> IO ()
setPropertyIO (k, v) j =
    if J.isUndefined x || J.isNull x
        || J.isUndefined k' || J.isNull k'
    then pure ()
    else js_unsafeSetProperty k v x
  where
    k' = J.pToJSVal k
    x = JE.toJS j

propertiesToObject :: [(J.JSString, J.JSVal)] -> JO.Object
propertiesToObject xs = (rnf names `seq` rnf values) `pseq` js_toJSObjectPure (unsafeCoerce names) (unsafeCoerce values)
  where
    (names, values) = unzip xs


objectToProperties :: JO.Object -> IO [(J.JSString, J.JSVal)]
objectToProperties obj = do
   props <- JO.listProps obj
   traverse (\k -> (\v -> (k, v)) <$> JO.unsafeGetProp k obj) props

#ifdef __GHCJS__

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$2[$1]"
  js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO J.JSVal

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$3[$1] = $2;"
  js_unsafeSetProperty :: J.JSString -> J.JSVal -> J.JSVal -> IO ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
foreign import javascript unsafe
  "hje$fromHsZipListJSVal($1, $2)"
  js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object

#else

-- | throws an exception if undefined or null
js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO J.JSVal
js_unsafeGetProperty _ _ = pure $ J.nullRef

-- | throws an exception if undefined or null
js_unsafeSetProperty :: J.JSString -> J.JSVal -> J.JSVal -> IO ()
js_unsafeSetProperty _ _ _ = pure ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object
js_toJSObjectPure _ _ = JOI.Object J.nullRef

#endif
