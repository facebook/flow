function foo() {
    var x;
    x.foo();
}

function bar() {
    var x:?{ bar():void; };
    if (x) x.bar(); // error: x is unitialized, so constant-condition
}

function qux(x?: number, y:string = "", z: void) { }

undefined['foo']; // Error
