import javascriptStringify from  'javascript-stringify';

// This has the side effect of running the haskell main after all javascript is loaded
import {} from '../.build-ghcjs/javascript-extras-test';

window.javascriptStringify = javascriptStringify;
