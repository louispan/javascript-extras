{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TupleSections #-}

module JavaScript.Extras.Property
    ( classNames
    , Property
    , getPropertyIO
    , setPropertyIO
    , fromProperties
    , toProperties
    ) where

import Control.DeepSeq
import Control.Parallel
import qualified Data.JSString as JS
import qualified GHC.Exts as Exts
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras.Cast as JE
import qualified JavaScript.Extras.JSRep as JE
import qualified JavaScript.Object as JO
import qualified JavaScript.Object.Internal as JOI
import Unsafe.Coerce

type Property = (J.JSString, JE.JSRep)

-- | Creates a JE.JSRep single string for "className" property from a list of (JSString, Bool)
-- Idea from https://github.com/JedWatson/classnames
classNames :: [(J.JSString, Bool)] -> JE.JSRep
classNames = JE.toJSR . JS.unwords . fmap fst . filter snd

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getPropertyIO :: JE.ToJS j => J.JSString -> j -> IO JE.JSRep
getPropertyIO k j = let k' = J.pToJSVal k
                        x = JE.toJS j
                  in if J.isUndefined x || J.isNull x
                         || J.isUndefined k' || J.isNull k'
                     then pure $ JE.JSRep J.nullRef
                     else js_unsafeGetProperty k x

-- | set a property of any JSVal
setPropertyIO :: JE.ToJS j => Property -> j -> IO ()
setPropertyIO (k, v) j = let k' = J.pToJSVal k
                             x = JE.toJS j
                    in if J.isUndefined x || J.isNull x
                          || J.isUndefined k' || J.isNull k'
                       then pure ()
                       else js_unsafeSetProperty k v x

fromProperties :: [Property] -> JO.Object
fromProperties xs =
    let (names, values) = unzip xs
    in (rnf names `seq` rnf values) `pseq` js_toJSObjectPure (unsafeCoerce names) (unsafeCoerce values)


toProperties :: JO.Object -> IO [Property]
toProperties obj = do
   props <- JO.listProps obj
   traverse (\k -> (\v -> (k, JE.JSRep v)) <$> JO.unsafeGetProp k obj) props

#ifdef __GHCJS__

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$2[$1]"
  js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO JE.JSRep

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$3[$1] = $2;"
  js_unsafeSetProperty :: J.JSString -> JE.JSRep -> J.JSVal -> IO ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
foreign import javascript unsafe
  "hje$fromHsZipListJSVal($1, $2)"
  js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object

#else

-- | throws an exception if undefined or null
js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO JE.JSRep
js_unsafeGetProperty _ _ = pure $ JE.JSRep J.nullRef

-- | throws an exception if undefined or null
js_unsafeSetProperty :: J.JSString -> JE.JSRep -> J.JSVal -> IO ()
js_unsafeSetProperty _ _ _ = pure ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object
js_toJSObjectPure _ _ = JOI.Object J.nullRef

#endif
