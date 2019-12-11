{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module JS.Data.Cast
    ( ToJS(..)
    , _toJS
    , FromJS(..)
    , _fromJS
    , viaJS
    , _viaJS
    ) where

import Control.Lens
import qualified Data.JSString as JS
import Data.Maybe
import qualified Data.Text as T
import GHC.Int
import GHC.Word
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Array.Internal as JAI
-- import qualified JavaScript.Object.Internal as JOI


-- | This provides a consistent way to convert to JSVal, with different semantics for Char.
-- In the Char's instance of ToJS, it converts to a string instead of integer - IMHO this is less surprising.
--
-- The other reason for this class is while GHCJS base provide both IsJSVal and PToJSVal to convert to jsval,
-- some types are instances of one or the other class.
-- This means you can't use the "Maybe a" instance of PToJSVal if it contains IsJSVal but not pToJSVal.
class ToJS a where
    -- | This is a pure conversion, so instances must be able to convert
    -- the same or equivalent JSVal each time.
    toJS :: a -> J.JSVal
    default toJS :: J.IsJSVal a => a -> J.JSVal
    toJS = J.jsval

instance ToJS J.JSVal where
    toJS = id

instance ToJS a => ToJS (Maybe a) where
    toJS Nothing = js_nothing
    toJS (Just a) = js_just (toJS a)

instance {-# OVERLAPPABLE #-} ToJS a => ToJS [a] where
    toJS xs = J.jsval (JAI.fromList (toJS <$> xs))

instance ToJS () where
    toJS _ = J.jsval $ JS.pack "()" -- ugly hack, actually a string
instance ToJS (J.Callback a)
instance ToJS (J.Export a)
-- instance ToJS (JAI.SomeJSArray m)
-- instance ToJS JOI.Object
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
instance {-# OVERLAPPING #-} ToJS String where
    toJS = J.pToJSVal
instance ToJS J.JSString

_toJS :: (ToJS a, Profunctor p, Contravariant f) => Optic' p f a J.JSVal
_toJS = to toJS

-- | This provides a consistent way to safely convert from JSVal.
-- The semantics is that if the return value is a Just, then the result is a valid value for that type.
-- Also, Nothing is also returned for values out of range. They are not silently truncated.
-- (Except for Float where there may be loss of precision) during conversion.
--
-- The following symmetries apply:
-- @
---fromJS . toJS = \v -> Just
-- toJS . fromJS = \v -> if validFromJS v then v else nullRef
-- @
--
-- This means @null@ or @undefined@ are valid 'validFromJS' for the instance for 'JSVal'.
--
-- The one exception is the instance for 'Maybe', where it is not symmetric.
-- The above rules is true for @Maybe a@, but not @Maybe (Maybe a)@
-- This is similar to the aeson issue https://github.com/bos/aeson/issues/376
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
    validFromJS :: J.JSVal -> Bool
    validFromJS = isJust . fromJS @a
    fromJS :: J.JSVal -> Maybe a

-- | All JSVal including @null@ and @undefined@ are 'validFromJS' 'JSVal'.
instance FromJS J.JSVal where
    validFromJS _ = True
    fromJS a = Just a

instance FromJS () where
    validFromJS = js_isValidUnit
    fromJS a | validFromJS @() a = Just ()
    fromJS _ = Nothing

instance FromJS a => FromJS (Maybe a) where
    validFromJS a = isJust (fromJS @(Maybe a) a)
    fromJS a = if js_isArray a
        then case js_unsafeLength a of
            0 -> Just Nothing
            1 -> case fromJS @a (js_unsafeHead a) of
                Nothing -> Nothing
                Just a -> Just (Just a)
            _ -> Nothing
        else Nothing

-- | Returns nothing if any element fails
instance {-# OVERLAPPABLE #-} FromJS a => FromJS [a] where
    validFromJS a = isJust (fromJS @(Maybe a) a)
    fromJS a = if js_isArray a
        then sequenceA (fromJS <$> (JAI.toList (JAI.SomeJSArray a)))
        else Nothing

instance FromJS Bool where
    validFromJS = js_isBool
    fromJS a | validFromJS @Bool a = J.pFromJSVal a
    fromJS _ = Nothing

-- | This will only succeed on a single character string
instance FromJS Char where
    validFromJS a = isJust (fromJS @Char a)
    fromJS a = if js_isString a
        then
            let a' = J.pFromJSVal a -- convert to JSString
                mb = JS.uncons a'
            in case mb of
                   Nothing -> Nothing
                   Just (h, t) ->
                       if JS.null t
                           then Just h
                           else Nothing
        else Nothing

instance FromJS Double where
    validFromJS = js_isNumber
    fromJS a | validFromJS @Double a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Float where
    validFromJS = js_isNumber
    fromJS a | validFromJS @Float a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int where
    validFromJS a = js_isNumber a && js_withinIntBounds a minBound maxBound
    fromJS a | validFromJS @Int a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int8 where
    validFromJS a = js_isNumber a && js_withinInt8Bounds a minBound maxBound
    fromJS a | validFromJS @Int8 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int16 where
    validFromJS a = js_isNumber a && js_withinInt16Bounds a minBound maxBound
    fromJS a | validFromJS @Int16 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Int32 where
    validFromJS a = js_isNumber a && js_withinInt32Bounds a minBound maxBound
    fromJS a | validFromJS @Int32 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word where
    validFromJS a = js_isNumber a && js_withinWordBounds a minBound maxBound
    fromJS a | validFromJS @Word a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word8 where
    validFromJS a = js_isNumber a && js_withinWord8Bounds a minBound maxBound
    fromJS a | validFromJS @Word8 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word16 where
    validFromJS a = js_isNumber a && js_withinWord16Bounds a minBound maxBound
    fromJS a | validFromJS @Word16 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS Word32 where
    validFromJS a = js_isNumber a && js_withinWord32Bounds a minBound maxBound
    fromJS a | validFromJS @Word32 a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS T.Text where
    validFromJS = js_isString
    fromJS a | validFromJS @T.Text a = J.pFromJSVal a
    fromJS _ = Nothing

instance {-# OVERLAPPING #-} FromJS String where
    validFromJS = js_isString
    fromJS a | validFromJS @String a = J.pFromJSVal a
    fromJS _ = Nothing

instance FromJS J.JSString where
    validFromJS = js_isString
    fromJS a | validFromJS @J.JSString a = J.pFromJSVal a
    fromJS _ = Nothing

_fromJS :: (FromJS a, Profunctor p, Contravariant f) => Optic' p f J.JSVal (Maybe a)
_fromJS = to fromJS

viaJS :: forall a b. (ToJS b, FromJS a) => b -> Maybe a
viaJS = fromJS . toJS

_viaJS :: forall a b f p. (ToJS b, FromJS a, Profunctor p, Contravariant f) => Optic' p f b (Maybe a)
_viaJS = to viaJS

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = [];"
    js_nothing :: J.JSVal

foreign import javascript unsafe
    "$r = [$1];"
    js_just :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = typeof $1 === 'object' && typeof $1.length === 'number';"
    js_isArray :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = $1.length;"
    js_unsafeLength :: J.JSVal -> Int

foreign import javascript unsafe
    "$r = $1[0];"
    js_unsafeHead :: J.JSVal -> J.JSVal

foreign import javascript unsafe
    "$r = typeof $1 === 'string' && $1 === '()';"
    js_isValidUnit :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'string';"
    js_isString :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'boolean';"
    js_isBool :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'number';"
    js_isNumber :: J.JSVal -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinIntBounds :: J.JSVal -> Int -> Int -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinInt8Bounds :: J.JSVal -> Int8 -> Int8 -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinInt16Bounds :: J.JSVal -> Int16 -> Int16 -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinInt32Bounds :: J.JSVal -> Int32 -> Int32 -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinWordBounds :: J.JSVal -> Word -> Word -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinWord8Bounds :: J.JSVal -> Word8 -> Word8 -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinWord16Bounds :: J.JSVal -> Word16 -> Word16 -> Bool

foreign import javascript unsafe
    "$r = ($1 >= $2) || ($1 <= $3);"
    js_withinWord32Bounds :: J.JSVal -> Word32 -> Word32 -> Bool

#else

js_nothing :: J.JSVal
js_nothing = J.nullRef

js_just :: J.JSVal -> J.JSVal
js_just = id

js_isArray :: J.JSVal -> Bool
js_isArray _ = False

js_unsafeLength :: J.JSVal -> Int
js_unsafeLength _ = 0

js_unsafeHead :: J.JSVal -> J.JSVal
js_unsafeHead = id

js_isValidUnit :: J.JSVal -> Bool
js_isValidUnit _ = False

js_isString :: J.JSVal -> Bool
js_isString _ = False

js_isBool :: J.JSVal -> Bool
js_isBool_ = False

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
