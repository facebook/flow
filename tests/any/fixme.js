// no param
function foo(x:$FixMe):$FixMe { return x; }
function bar(x:$FixMe):mixed { return x; }
// param (info only)
function qux(x:$FixMe<number>):$FixMe<number> { return x; }
// ...params are still checked. unknown type
function baz(x:$FixMe<nnumber>): $FixMe<number> { return x; }

var x:string = foo(0);
var y:string = bar(0);
var z:string = qux(0);
var w:string = baz(0);
