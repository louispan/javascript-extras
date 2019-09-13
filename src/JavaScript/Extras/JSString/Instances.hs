{-# OPTIONS_GHC -Wno-orphans #-}

module JavaScript.Extras.JSString.Instances where

import Data.Hashable
import qualified Data.JSString as J

instance Hashable J.JSString where
    hashWithSalt s x = hashWithSalt s (J.unpack x)
    hash = hash . J.unpack