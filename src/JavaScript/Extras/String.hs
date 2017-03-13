module JavaScript.Extras.String where

import JavaScript.Extras.Recast as JE
import Data.Text as T
import qualified GHCJS.Types as J

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a JSString
-- Less verbose version of'toJS @JSString'
strJS :: J.JSString -> J.JSVal
strJS = JE.toJS

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a Data.Text
-- Less verbose version of'toJS @Text'
txtJS :: T.Text -> J.JSVal
txtJS = JE.toJS

-- | This makes it easier to use OverloadedStrings with inputs that accept a JSVal that could be a [String]
-- Less verbose version of'toJS @Jtring'
strJS' :: [Char] -> J.JSVal
strJS' = JE.toJS
