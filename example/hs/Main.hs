{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

#ifdef __GHCJS__

import qualified Data.JSString as J
import qualified JS.Data
import qualified JS.Data.Unsafe ()

test :: JSVal -> IO ()
test x = do
    let str = show x
        x' = read str :: JSVal
        str' = show x'
    js_write $ "Test: " `mappend` (J.pack str) `mappend` " == " `mappend` (J.pack str')

main :: IO ()
main = do
    test js_int
    test js_obj
    cb <- syncCallback' (pure (toJS js_int))
    test (jsval cb)
    releaseCallback cb
    test (jsval cb)

foreign import javascript unsafe
  "document.write('<p>' + $1 + '</p>')"
  js_write :: JS.JSString -> IO ()

foreign import javascript unsafe
  "675324"
  js_int :: JSVal

foreign import javascript unsafe
  "{ 'hello': 'world' }"
  js_obj :: JSVal

#else

main :: IO ()
main = pure ()

#endif
