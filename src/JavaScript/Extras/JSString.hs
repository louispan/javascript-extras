module JavaScript.Extras.JSString where

import qualified Data.JSString as J

-- | Convenience function when using OverloadedStrings
jsstr :: J.JSString -> J.JSString
jsstr = id
