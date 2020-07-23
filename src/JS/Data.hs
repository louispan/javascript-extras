module JS.Data
    ( JSVal
    , nullRef
    , JSString
    , Callback
    , module JS.Data.Object
    , module JS.Data.Cast
    ) where

import GHCJS.Foreign.Callback
import GHCJS.Types
import JS.Data.Cast
import JS.Data.Instances ()
import JS.Data.Object
