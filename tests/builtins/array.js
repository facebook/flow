var a = ["..."];
var b = a.map (function (x) { return 0; });
var c: string = b[0]; // error: number !~> string

var array: Array<string> = [];
function f() {
    array = array.map (function () { return "..."; });
    var x:number = array[0]; // error: string !~> number
}

var Foo = require('./genericfoo');
var foo = new Foo<mixed>();
function g() {
    var foo1 = foo.map (function() { return "..."; });
    var x:number = foo1.get(); // error: string !~> number
}
