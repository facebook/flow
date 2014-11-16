function foo(): number {
    var x: ?number = null;
    return x != null? x : 0;
}

function bar(): number {
    var x: ?number = null;
    return x || 0;
}

function qux(): boolean {
    var x: ?number = null;
    return x != null && x > 10;
}

function waldo(): number {
    var x: ?number = null;
    return x != null && x;
}

function corge() {
    var x: ?number = null;
    if (x && x > 0) { }
}
