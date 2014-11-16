var a = ["..."];
var b = a.map (function (x) { return 0; });
var c: string = b[0];

var array = [];
function f() {
    array = array.map (function () { return "..."; });
    var x:number = array[0];
}

var Foo = require('./genericfoo');
var foo = new Foo();
function g() {
    var foo1 = foo.map (function() { return "..."; });
    var x:number = foo1.get();
    foo = foo1;
}
