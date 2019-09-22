{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module JavaScript.Extras.Cast
    ( ToJS(..)
    , toJS_
    , FromJS(..)
    , fromJS_
    ) where

import Control.Lens
import qualified Data.JSString as JS
import Data.Maybe
import qualified Data.Text as T
import GHC.Int
import GHC.Word
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Foreign.Internal as JFI
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
instance ToJS String where
    toJS = J.pToJSVal
instance ToJS J.JSString
instance ToJS a => ToJS (Maybe a) where
    toJS Nothing  = J.nullRef
    toJS (Just a) = toJS a

toJS_ :: ToJS a => Getting J.JSVal a J.JSVal
toJS_ = to toJS

-- | This provides a consistent way to safely convert from JSVal.
-- The semantics is that if the return value is a Just, then the JSVal is not a null value.
-- Also, Nothing is also returned for values out of range. They are not silently truncated.
-- (Except for Float where there may be loss of precision) during conversion.
--
-- The reason for this class is because GHCJS.Marshal.fromJSVal and GHCJS.Marshal.pFromJSVal
-- are not safe to use as it assumes that the JSVal are of the correct type and not null.
-- (https://github.com/ghcjs/ghcjs-base/issues/87).
-- The safe way to convert from JSVal is to use JavaScript.Cast or to use the 'Maybe a' instance of FromJSVal,
-- ie @fromJSVal :: JSVal -> IO (Maybe (Maybe a))@, which is a bit more awkward to use, and requires IO.
-- Also, Javascript.Cast doesn't have much instances, and it hardcodes the instance detection method
-- to javascript `isinstance` which is not sufficient for complex types (https://github.com/ghcjs/ghcjs-base/issues/86).
--
-- It is actually safe to convert from JSVal without IO because every JSVal is a copy of a value or reference.
-- The copy never change, so the conversion will always convert to the same result/object every time.
class FromJS a where
    validInstance :: J.JSVal -> Bool
    validInstance = isJust . fromJS @a
    fromJS :: J.JSVal -> Maybe a

instance FromJS J.JSVal where
    validInstance a = J.isUndefined a || J.isNull a
    fromJS a | validInstance @J.JSVal a = Nothing
    fromJS a = Just a

instance FromJS (JAI.SomeJSArray m) where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONArray
    fromJS a | validInstance @(JAI.SomeJSArray m) a = Just $ JAI.SomeJSArray a
    fromJS _ = Nothing

instance FromJS JOI.Object where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONObject
    fromJS a | validInstance @(JOI.Object) a = Just $ JOI.Object a
    fromJS _ = Nothing

instance FromJS Bool where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONBool
    fromJS a | validInstance @Bool a = J.pFromJSVal a
    fromJS _ = Nothing

-- | This will only succeed on a single character string
instance FromJS Char where
    validInstance a = isJust (fromJS @Char a)
    fromJS a =
        case JFI.jsonTypeOf a of
            JFI.JSONString ->
                let a' = J.pFromJSVal a -- convert to JSString
                    mb = JS.uncons a'
                in case mb of
                       Nothing -> Nothing
                       Just (h, t) ->
                           if JS.null t
                               then Just h
                               else Nothing
            _ -> Nothing

instance FromJS Double where
    validInstance a = let t = JFI.jsonTypeOf a
      in t == JFI.JSONInteger || t == JFI.JSONFloat
    fromJS a | validInstance @Double a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Float where
    validInstance a = let t = JFI.jsonTypeOf a
      in t == JFI.JSONInteger || t == JFI.JSONFloat
    fromJS a | validInstance @Float a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinIntBounds a minBound maxBound
    fromJS a | validInstance @Int a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int8 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt8Bounds a minBound maxBound
    fromJS a | validInstance @Int8 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int16 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt16Bounds a minBound maxBound
    fromJS a | validInstance @Int16 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int32 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinInt32Bounds a minBound maxBound
    fromJS a | validInstance @Int32 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWordBounds a minBound maxBound
    fromJS a | validInstance @Word a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word8 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord8Bounds a minBound maxBound
    fromJS a | validInstance @Word8 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word16 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord16Bounds a minBound maxBound
    fromJS a | validInstance @Word16 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word32 where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONInteger && js_withinWord32Bounds a minBound maxBound
    fromJS a | validInstance @Word32 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS T.Text where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONString
    fromJS a | validInstance @T.Text a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS String where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONString
    fromJS a | validInstance @String a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS J.JSString where
    validInstance a = JFI.jsonTypeOf a == JFI.JSONString
    fromJS a | validInstance @J.JSString a = J.pFromJSVal a
    fromJS _ = Nothing

fromJS_ :: FromJS a => Getting (Maybe a) J.JSVal (Maybe a)
fromJS_ = to fromJS

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
