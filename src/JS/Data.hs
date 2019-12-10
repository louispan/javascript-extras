module JS.Data
    ( JSVal
    , nullRef
    , JSString
    , Callback
    , module JS.Data.Object
    , module JS.Data.Cast
    , module JS.Data.Console
    ) where

import GHCJS.Foreign.Callback
import GHCJS.Types
import JS.Data.Cast
import JS.Data.Instances ()
import JS.Data.Console
import JS.Data.Object
