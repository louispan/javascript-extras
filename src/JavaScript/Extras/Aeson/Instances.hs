{-# OPTIONS_GHC -fno-warn-orphans #-}

module JavaScript.Extras.Aeson.Instances where

import Data.Aeson
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
