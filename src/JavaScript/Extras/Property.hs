{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GHCForeignImportPrim #-}

module JavaScript.Extras.Property
    ( Property
    , getProperty
    , setProperty
    , fromProperties
    , toProperties
    ) where

import Control.DeepSeq
import Control.Parallel
import Data.Coerce
import qualified GHC.Exts as Exts
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Object as JO
import qualified JavaScript.Object.Internal as JOI
import qualified JavaScript.Extras.JSVar as JE
import Unsafe.Coerce

type Property = (J.JSString, JE.JSVar)

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getProperty :: Coercible a J.JSVal => J.JSString -> a -> IO JE.JSVar
getProperty k a = let k' = J.pToJSVal k
                      x = coerce a
                  in if J.isUndefined x || J.isNull x
                         || J.isUndefined k' || J.isNull k'
                     then pure $ JE.JSVar J.nullRef
                     else js_unsafeGetProperty k x

-- | set a property of any JSVal
setProperty :: Coercible a J.JSVal => Property -> a -> IO ()
setProperty (k, v) a = let k' = J.pToJSVal k
                           x = coerce a
                    in if J.isUndefined x || J.isNull x
                          || J.isUndefined k' || J.isNull k'
                       then pure ()
                       else js_unsafeSetProperty k v x

fromProperties :: [Property] -> JO.Object
fromProperties xs =
    let (names, props) = unzip xs
    in (rnf names `seq` rnf props) `pseq` js_toJSObjectPure (unsafeCoerce names) (unsafeCoerce props)


toProperties :: JO.Object -> IO [Property]
toProperties obj = do
   props <- JO.listProps obj
   traverse (\k -> (\v -> (k, JE.JSVar v)) <$> JO.unsafeGetProp k obj) props

#ifdef __GHCJS__

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$2[$1]"
  js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO JE.JSVar

-- | throws an exception if undefined or null
foreign import javascript unsafe
  "$3[$1] = $2;"
  js_unsafeSetProperty :: J.JSString -> JE.JSVar -> J.JSVal -> IO ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
foreign import javascript unsafe "hje$fromHsZipListJSVal($1, $2)"
  js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object

#else

-- | throws an exception if undefined or null
js_unsafeGetProperty :: J.JSString -> J.JSVal -> IO JE.JSVar
js_unsafeGetProperty _ _ = pure $ JE.JSVar J.nullRef

-- | throws an exception if undefined or null
js_unsafeSetProperty :: J.JSString -> JE.JSVar -> J.JSVal -> IO ()
js_unsafeSetProperty _ _ _ = pure ()

-- | zip list of string and JSVal to object, lists must have been completely forced first
-- Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
js_toJSObjectPure :: Exts.Any -> Exts.Any -> JO.Object
js_toJSObjectPure _ _ = JOI.Object J.nullRef

#endif
