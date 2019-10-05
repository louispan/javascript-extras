#include <ghcjs/rts.h>

// zip list of string and JSVal to object, lists must have been completely forced first
// Using the idea from JavaScript.Array.Internal.fromList h$fromHsListJSVal
function hje$fromHsZipListJSVal(namess, xs) {
    var obj = {};
    while(IS_CONS(namess) && IS_CONS(xs)) {
        obj[JSVAL_VAL(CONS_HEAD(namess))] = JSVAL_VAL(CONS_HEAD(xs));
        namess = CONS_TAIL(namess);
        xs = CONS_TAIL(xs);
    }
    return obj;
}

// stringify can fail and return
var hge$stringify_ = null;
function hje$stringify(v) {
    if (!hge$stringify_) {
        hge$stringify_ = require('javascript-stringify');
    }
    return hge$stringify_(v, null, null, { references: true })
}

// Injection attack! Use with care
function hje$unstringify(str) {
    eval("var ret=" + str);
    return ret;
}
