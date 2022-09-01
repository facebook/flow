// @flow

function foo(b: boolean) {
    var x = b? 0 : null;
    while (typeof x == "string" || typeof x == "number") {
        var y:string = x; // Error
        x = false; // Constrain writes error
    }
    var z:string = x; // Error
}

function bar(b: boolean) {
    var x = b? 0 : null;
    do {
        var y:string = x;  // Error
        x = false; // Constrain writes error
    } while (x === null);
    var z:string = x;  // Error
}

function maybe_throw() { }
function qux() {
    var x = 0;
    try {
        maybe_throw();
        x = "hello"; // Constrain writes error
    } catch (e) {
        maybe_throw();
        x = "hello"; // Constrain writes error
    } finally {
        // NOTE: the values understood to flow to x at this point
        // include the number 42 written downstream;
        // so if we did y:string, we would get at least a spurious error
        // (among other reasonable errors caused by values written upstream)
        var y:number = x;  // Error
        x = 42;
    }
    var z:string = x;  // Error
}

function corge(b: boolean) {
    for (var x = b? 0 : null;
         typeof x == "string" || typeof x == "number";
         x = false) {
        var y:string = x;  // Error
    }
    var z:string = x; // Error
}

function waldo() {
    var o = {};
    var x = false;
    for (x in o) { // Constrain writes error
        // Constrain writes error
        x = 0; // commenting this out would propagate x:string downstream
    }
    var z:number = x; // Error
}

// regression test: bring a global into scope by testing it.
// this has no refinement consequences and is error-free.
// the way we currently cache global lookups causes uneven
// distribution of the global's entries at path merge time,
// so we need to recognize that it's legit rather than an
// internal error.
//
function global_in_conditional0(x: number) {
    // merge_env
    if (x != 0) {
        if (BAZ) {
        }
    }
}

function global_in_conditional2(x: number) {
    // copy_env
    for (var i = 0; i < 100; i++) {
        if (BAZ) {
        }
    }
}
