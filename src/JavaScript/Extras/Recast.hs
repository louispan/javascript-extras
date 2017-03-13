{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module JavaScript.Extras.Recast
    ( ToJS(..)
    , FromJS(..)
    ) where

import qualified Data.JSString as JS
import qualified Data.Text as T
import GHC.Int
import GHC.Word
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Foreign.Internal as JFI
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified JavaScript.Array.Internal as JAI
import qualified JavaScript.Object.Internal as JOI

-- | This provides a consistent way to convert to JSVal, with different semantics for Char.
-- In the Char's instance of ToJS, it converts to a string instead of integer - IMHO this is less surprising.
--
-- The other reason for this class is while GHCJS base provide both IsJSVal and PToJSVal to convert to jsval,
-- some types are instances of one or the other class.
-- This means you can't use the "Maybe a" instance of PToJSVal if it contains IsISJVal but not pToJSVal.
class ToJS a where
    -- | This is a pure conversion, so instances must be able to convert
    -- the same or equivalent JSVal each time.
    toJS :: a -> J.JSVal
    default toJS :: J.IsJSVal a => a -> J.JSVal
    toJS = J.jsval

instance ToJS J.JSVal where
    toJS = id
instance ToJS (J.Callback a)
instance ToJS (J.Export a)
instance ToJS (J.Nullable a) where
    toJS (J.Nullable a) = a
instance ToJS (JAI.SomeJSArray m)
instance ToJS JOI.Object
instance ToJS Bool where
    toJS = J.pToJSVal
-- | Char instance converts to string
instance ToJS Char where
    toJS a = J.pToJSVal [a]
instance ToJS Double where
    toJS = J.pToJSVal
instance ToJS Float where
    toJS = J.pToJSVal
instance ToJS Int where
    toJS = J.pToJSVal
instance ToJS Int8 where
    toJS = J.pToJSVal
instance ToJS Int16 where
    toJS = J.pToJSVal
instance ToJS Int32 where
    toJS = J.pToJSVal
instance ToJS Word where
    toJS = J.pToJSVal
instance ToJS Word8 where
    toJS = J.pToJSVal
instance ToJS Word16 where
    toJS = J.pToJSVal
instance ToJS Word32 where
    toJS = J.pToJSVal
instance ToJS T.Text where
    toJS = J.pToJSVal
instance ToJS [Char] where
    toJS = J.pToJSVal
instance ToJS J.JSString
instance ToJS a => ToJS (Maybe a) where
    toJS Nothing  = J.nullRef
    toJS (Just a) = toJS a

-- | This provides a consistent way to safely convert from JSVal.
-- The semantics is that if the return value is a Just, then the JSVal is not a null value.
-- Also, Nothing is also returned for values out of range. They are not silently truncated.
-- (Except for Float where there may be loss of precision) during conversion.
--
-- The reason for this class is because GHCJS.Marshal.fromJSVal and GHCJS.Marshal.pFromJSVal
-- are not safe to use as it assumes that the JSVal are of the correct type and not null.
-- (https://github.com/ghcjs/ghcjs-base/issues/87).
-- The safe way to convert from JSVal is to use JavaScript.Cast or to use the 'Maybe a' instance of FromJSVal,
-- ie @fromJSVal :: JSVal -> IO (Maybe (Maybe a))@, which is a bit more awkward to use.
-- Also, Javascript.Cast doesn't have much instances, and it hardcodes the instance detection method
-- to javascript `isinstance` which is not sufficient for complex
-- types (https://github.com/ghcjs/ghcjs-base/issues/86).
class FromJS a where
    -- | This is an IO because since JSVal is mutable, this function may different results
    -- for the same JSVal at later points in time.
    fromJS :: J.JSVal -> IO (Maybe a)

instance FromJS J.JSVal where
    fromJS a | J.isUndefined a || J.isNull a = pure Nothing
    fromJS a = pure $ Just a

instance FromJS (JAI.SomeJSArray m) where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONArray = pure . Just $ JAI.SomeJSArray a
    fromJS _ = pure Nothing

instance FromJS JOI.Object where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONObject = pure . Just $ JOI.Object a
    fromJS _ = pure Nothing

instance FromJS Bool where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONBool = J.fromJSVal a
    fromJS _ = pure Nothing

-- | This will only succeed on a single character string
instance FromJS Char where
    fromJS a =
        case JFI.jsonTypeOf a of
            JFI.JSONString ->
                let a' = J.pFromJSVal a -- convert to JSString
                    mb = JS.uncons a'
                in case mb of
                       Nothing -> pure Nothing
                       Just (h, t) ->
                           if JS.null t
                               then pure $ Just h
                               else pure Nothing
            _ -> pure Nothing

instance FromJS Double where
    fromJS a = let t = JFI.jsonTypeOf a
               in if t == JFI.JSONInteger || t == JFI.JSONFloat
                      then J.fromJSVal a
                      else pure Nothing

instance FromJS Float where
    fromJS a = let t = JFI.jsonTypeOf a
               in if t == JFI.JSONInteger || t == JFI.JSONFloat
                      then J.fromJSVal a
                      else pure Nothing

instance FromJS Int where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinIntBounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Int8 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt8Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Int16 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt16Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Int32 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt32Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Word where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWordBounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Word8 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord8Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Word16 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord16Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS Word32 where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord32Bounds a minBound maxBound = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS T.Text where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONString = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS [Char] where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONString = J.fromJSVal a
    fromJS _ = pure Nothing

instance FromJS J.JSString where
    fromJS a | JFI.jsonTypeOf a == JFI.JSONString = J.fromJSVal a
    fromJS _ = pure Nothing

#ifdef __GHCJS__

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinIntBounds :: J.JSVal -> Int -> Int -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinInt8Bounds :: J.JSVal -> Int8 -> Int8 -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinInt16Bounds :: J.JSVal -> Int16 -> Int16 -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinInt32Bounds :: J.JSVal -> Int32 -> Int32 -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinWordBounds :: J.JSVal -> Word -> Word -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinWord8Bounds :: J.JSVal -> Word8 -> Word8 -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinWord16Bounds :: J.JSVal -> Word16 -> Word16 -> Bool

foreign import javascript unsafe
  "($1 >= $2) || ($1 <= $3)"
  js_withinWord32Bounds :: J.JSVal -> Word32 -> Word32 -> Bool

#else

js_withinIntBounds :: J.JSVal -> Int -> Int -> Bool
js_withinIntBounds _ _ _ = False

js_withinInt8Bounds :: J.JSVal -> Int8 -> Int8 -> Bool
js_withinInt8Bounds _ _ _ = False

js_withinInt16Bounds :: J.JSVal -> Int8 -> Int8 -> Bool
js_withinInt16Bounds _ _ _ = False

js_withinInt32Bounds :: J.JSVal -> Int8 -> Int8 -> Bool
js_withinInt32Bounds _ _ _ = False

js_withinWordBounds :: J.JSVal -> Word -> Word -> Bool
js_withinWordBounds _ _ _ = False

js_withinWord8Bounds :: J.JSVal -> Word8 -> Word8 -> Bool
js_withinWord8Bounds _ _ _ = False

js_withinWord16Bounds :: J.JSVal -> Word16 -> Word16 -> Bool
js_withinWord16Bounds _ _ _ = False

js_withinWord32Bounds :: J.JSVal -> Word32 -> Word32 -> Bool
js_withinWord32Bounds _ _ _ = False

#endif
