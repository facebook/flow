function foo(x:string) { }

var a = [0];
var b = a.map(function (x) { foo(x); return "" + x; });

var c: number = a[0];
var d: number = b[0];

var e:Array<string> = a.reverse();

var f = [""];
var g:number = f.map(function () { return 0; })[0];
