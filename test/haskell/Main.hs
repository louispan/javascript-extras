{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

#ifdef __GHCJS__

import qualified Data.JSString as JS
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified JavaScript.Extras.JSVar.Unsafe as JE

test :: JE.JSVar -> IO ()
test x = do
    let str = show x
        x' = read str :: JE.JSVar
        str' = show x'
    js_write $ "Test: " `mappend` (JS.pack str) `mappend` " == " `mappend` (JS.pack str')

main :: IO ()
main = do
    test js_int
    test js_obj
    cb <- J.syncCallback' (pure (JE.toJS js_int))
    test (JE.JSVar (J.jsval cb))
    J.releaseCallback cb
    test (JE.JSVar (J.jsval cb))

foreign import javascript unsafe
  "document.write('<p>' + $1 + '</p>')"
  js_write :: JS.JSString -> IO ()

foreign import javascript unsafe
  "5"
  js_int :: JE.JSVar

foreign import javascript unsafe
  "{ 'hello': 'world' }"
  js_obj :: JE.JSVar

#else

main :: IO ()
main = pure ()

#endif
