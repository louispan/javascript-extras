{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

#ifdef __GHCJS__

import qualified Data.JSString as JS
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified JavaScript.Extras.JSVal.Unsafe ()

test :: J.JSVal -> IO ()
test x = do
    let str = show x
        x' = read str :: J.JSVal
        str' = show x'
    js_write $ "Test: " `mappend` (JS.pack str) `mappend` " == " `mappend` (JS.pack str')

main :: IO ()
main = do
    test js_int
    test js_obj
    -- cb <- J.syncCallback' (pure (JE.toJS js_int))
    -- test (J.jsval cb)
    -- J.releaseCallback cb
    -- test (J.jsval cb)

foreign import javascript unsafe
  "document.write('<p>' + $1 + '</p>')"
  js_write :: JS.JSString -> IO ()

foreign import javascript unsafe
  "675324"
  js_int :: J.JSVal

foreign import javascript unsafe
  "{ 'hello': 'world' }"
  js_obj :: J.JSVal

#else

main :: IO ()
main = pure ()

#endif
