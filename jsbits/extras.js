// Node module dependencies
// Example package.json:
// {
//   "dependencies": {
//     "javascript-stringify": "^1.6.0"
//   }
// }

#include <ghcjs/rts.h>

// zip list of string and JSVal to object, lists must have been completely forced first
// Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
function hje$fromHsZipListJSVal(names, xs) {
    var obj = {};
    while(IS_CONS(names) && IS_CONS(xs)) {
        obj[JSVAL_VAL(CONS_HEAD(names))] = JSVAL_VAL(CONS_HEAD(xs));
        names = CONS_TAIL(names);
        xs = CONS_TAIL(xs);
    }
    return obj;
}

var hge$javascriptStringify_ = null;
function hje$stringify(v) {
    if (!hge$javascriptStringify_) {
        hge$javascriptStringify_ = require('javascript-stringify');
    }
    return hge$javascriptStringify_(v, null, null, { references: true })
}

// Injection attack! Use with care
function hje$unstringify(str) {
    eval("var ret=" + str);
    return ret;
}
