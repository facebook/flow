function foo(b) {
    var x = b? 0 : null;
    while (typeof x == "string" || typeof x == "number") {
        var y:string = x;
        x = false;
    }
    var z:string = x;
}

function bar(b) {
    var x = b? 0 : null;
    do {
        var y:string = x;
        x = false;
    } while (x === null);
    var z:string = x;
}

function maybe_throw() { }
function qux() {
    var x = 0;
    try {
        maybe_throw();
        x = "hello";
    } catch (e) {
        maybe_throw();
        x = "hello";
    } finally {
        // NOTE: the values understood to flow to x at this point
        // include the number 42 written downstream;
        // so if we did y:string, we would get at least a spurious error
        // (among other reasonable errors caused by values written upstream)
        var y:number = x;
        x = 42;
    }
    var z:string = x;
}

function corge(b) {
    for (var x = b? 0 : null;
         typeof x == "string" || typeof x == "number";
         x = false) {
        var y:string = x;
    }
    var z:string = x;
}

function waldo() {
    var o = {};
    var x = false;
    for (x in o) {
        x = 0; // commenting this out would propagate x:string downstream
    }
    var z:number = x;
}
