{-# LANGUAGE CPP #-}

module JS.Data.Console where

import GHCJS.Types

consoleInfo1 :: JSVal -> IO ()
consoleInfo1 = js_consoleInfo1

consoleWarn1 :: JSVal -> IO ()
consoleWarn1 = js_consoleWarn1

consoleError1 :: JSVal -> IO ()
consoleError1 = js_consoleError1

consoleInfo2 :: JSVal -> JSVal -> IO ()
consoleInfo2 = js_consoleInfo2

consoleWarn2 :: JSVal -> JSVal -> IO ()
consoleWarn2 = js_consoleWarn2

consoleError2 :: JSVal -> JSVal -> IO ()
consoleError2 = js_consoleError2

consoleInfo3 :: JSVal -> JSVal -> JSVal -> IO ()
consoleInfo3 = js_consoleInfo3

consoleWarn3 :: JSVal -> JSVal -> JSVal -> IO ()
consoleWarn3 = js_consoleWarn3

consoleError3 :: JSVal -> JSVal -> JSVal -> IO ()
consoleError3 = js_consoleError3

#ifdef __GHCJS__

foreign import javascript unsafe
    "console.info($1);"
    js_consoleInfo1 :: JSVal -> IO ()

foreign import javascript unsafe
    "console.warn($1);"
    js_consoleWarn1 :: JSVal -> IO ()

foreign import javascript unsafe
    "console.error($1);"
    js_consoleError1 :: JSVal -> IO ()

foreign import javascript unsafe
    "console.info($1, $2);"
    js_consoleInfo2 :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "console.warn($1, $2);"
    js_consoleWarn2 :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "console.error($1, $2);"
    js_consoleError2 :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "console.info($1, $2, $3);"
    js_consoleInfo3 :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "console.warn($1, $2, $3);"
    js_consoleWarn3 :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "console.error($1, $2, $3);"
    js_consoleError3 :: JSVal -> JSVal -> JSVal -> IO ()

#else

js_consoleInfo1 :: JSVal -> IO ()
js_consoleInfo1 _ = pure ()

js_consoleWarn1 :: JSVal -> IO ()
js_consoleWarn1 _ = pure ()

js_consoleError1 :: JSVal -> IO ()
js_consoleError1 _ = pure ()

js_consoleInfo2 :: JSVal -> JSVal -> IO ()
js_consoleInfo2 _ _ = pure ()

js_consoleWarn2 :: JSVal -> JSVal -> IO ()
js_consoleWarn2 _ _ = pure ()

js_consoleError2 :: JSVal -> JSVal -> IO ()
js_consoleError2 _ _ = pure ()

js_consoleInfo3 :: JSVal -> JSVal -> JSVal -> IO ()
js_consoleInfo3 _ _ _ = pure ()

js_consoleWarn3 :: JSVal -> JSVal -> JSVal -> IO ()
js_consoleWarn3 _ _ _ = pure ()

js_consoleError3 :: JSVal -> JSVal -> JSVal -> IO ()
js_consoleError3 _ _ _ = pure ()

#endif
