/* @flow */
function foo(x:string) { }

var a = [0];
var b = a.map(function (x) { foo(x); return "" + x; });

var c: number = a[0];
var d: number = b[0];

var e:Array<string> = a.reverse();

var f = [""];
var g:number = f.map(function () { return 0; })[0];

var h: Array<number> = [1,2,3];
var i: Array<string> = ['a', 'b', 'c'];
var j: Array<number | string> = h.concat(i);
var k: Array<number> = h.concat(h);
var l: Array<number> = h.concat(1,2,3);
var m: Array<number | string> = h.concat('a', 'b', 'c');
var n: Array<number> = h.concat('a', 'b', 'c'); // Error
