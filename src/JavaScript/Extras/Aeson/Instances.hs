{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JavaScript.Extras.Aeson.Instances where

import Data.Aeson
import Data.Aeson.Applicative
import qualified Data.Aeson.Encoding as E
import qualified Data.JSString as J
import qualified Data.JSString.Text as J

instance FromJSON J.JSString where
    parseJSON = withText "JSString" (pure . J.textToJSString)
    {-# INLINE parseJSON #-}

instance ToJSON J.JSString where
    toJSON = String . J.textFromJSString
    {-# INLINE toJSON #-}

    toEncoding = E.text . J.textFromJSString
    {-# INLINE toEncoding #-}

instance Applicative m => AToJSON m J.JSString where
    atoEncoding = pure . toEncoding

instance Applicative m => AFromJSON m J.JSString where
    aparseJSON = fmap pure . parseJSON